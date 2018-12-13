package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable, N.Program)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable, N.Program) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach { case (module) =>
      val defNames = module.defs.groupBy(_.name)
      defNames.foreach { case (name, defs) =>
        if (defs.size > 1) {
          fatal(s"Two definitions named $name in module", defs.head.position)
        }
      }
    }

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach { case module =>
      module.defs.foreach {
        case abstractClass: N.AbstractClassDef => table.addType(module.name, abstractClass.name)
        case _ =>
      }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach { case module =>
      module.defs.foreach {
        case caseClass: N.CaseClassDef =>
          if (table.getType(module.name, caseClass.parent).isEmpty) {
            fatal(s"Parent type must be in the same class", caseClass)
          } else {
            table.addConstructor(module.name, caseClass.name, caseClass.fields.map(transformType(_, module.name)), table.getType(module.name, caseClass.parent).get)
          }
        case _ =>
      }
    }

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach { case module =>
      module.defs.foreach {
        case funDef: N.FunDef =>
          table.addFunction(module.name, funDef.name, funDef.params.map(param => transformType(param.tt, module.name)), transformType(funDef.retType, module.name))
        case _ =>
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = {
      df match {
        case N.AbstractClassDef(name) =>
          val Some(identifier) = table.getType(module, name)
          S.AbstractClassDef(identifier)

        case N.CaseClassDef(name, _, _) =>
          val Some((identifier, constrSig)) = table.getConstructor(module, name)
          val (argTypes, parent) = (constrSig.argTypes, constrSig.parent)

          S.CaseClassDef(
            identifier,
            argTypes.map(S.TypeTree(_)),
            parent
          )

        case fd: N.FunDef =>
          transformFunDef(fd, module)
      }
    }.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body, doc) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map())),
        doc
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case N.WildcardPattern() => (S.WildcardPattern(), Nil)

              case pattern@N.IdPattern(idPattern) =>
                if (locals.contains(idPattern)) {
                  fatal(s"Multiples definitions for $idPattern", pattern)
                } else {
                  val name = Identifier.fresh(idPattern)
                  (S.IdPattern(name).setPos(pat.position), List((idPattern, name)))
                }
              case N.LiteralPattern(lit) =>
                (S.LiteralPattern(lit.asInstanceOf[S.Literal[lit.type]]).setPos(pat.position), Nil)


              case N.CaseClassPattern(constr, args) =>
                val caseClass = table.getConstructor(constr.module.getOrElse(module), constr.name)
                if (caseClass.isEmpty || caseClass.get._2.argTypes.size != args.size) {
                  fatal(s"No constructor $constr with such signature", pat.position)
                } else {
                  val transformedArgs = args.map(transformPattern(_))
                  (S.CaseClassPattern(caseClass.get._1, transformedArgs.map(_._1)).setPos(pat.position), transformedArgs.flatMap(_._2))
                }
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            moreLocals.groupBy(_._1).foreach {
              case names => if (names._2.size > 1) {
                val name = names._1
                fatal(s"Duplicate variable $name in pattern.", cse)
              }
            }
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals++moreLocals))).setPos(cse.position)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        case variable@N.Variable(name) =>
          if (locals.contains(name)) {
            S.Variable(locals(name))
          } else if (params.contains(name)) {
            S.Variable(params(name))
          } else {
            fatal(s"Variable $name is never declared", variable)
          }

        case N.IntLiteral(value) =>
          S.IntLiteral(value)

        case N.BooleanLiteral(value) =>
          S.BooleanLiteral(value)

        case N.StringLiteral(value) =>
          S.StringLiteral(value)

        case N.UnitLiteral() =>
          S.UnitLiteral()

        case N.Plus(lhs, rhs) =>
          S.Plus(transformExpr(lhs), transformExpr(rhs))

        case N.Minus(lhs, rhs) =>
          S.Minus(transformExpr(lhs), transformExpr(rhs))

        case N.Times(lhs, rhs) =>
          S.Times(transformExpr(lhs), transformExpr(rhs))

        case N.Div(lhs, rhs) =>
          S.Div(transformExpr(lhs), transformExpr(rhs))

        case N.Mod(lhs, rhs) =>
          S.Mod(transformExpr(lhs), transformExpr(rhs))

        case N.LessThan(lhs, rhs) =>
          S.LessThan(transformExpr(lhs), transformExpr(rhs))

        case N.LessEquals(lhs, rhs) =>
          S.LessEquals(transformExpr(lhs), transformExpr(rhs))

        case N.And(lhs, rhs) =>
          S.And(transformExpr(lhs), transformExpr(rhs))

        case N.Or(lhs, rhs) =>
          S.Or(transformExpr(lhs), transformExpr(rhs))

        case N.Equals(lhs, rhs) =>
          S.Equals(transformExpr(lhs), transformExpr(rhs))

        case N.Concat(lhs, rhs) =>
          S.Concat(transformExpr(lhs), transformExpr(rhs))

        case N.Not(e) =>
          S.Not(transformExpr(e))

        case N.Neg(e) =>
          S.Neg(transformExpr(e))

        case N.Call(qname, args) =>
          val function = table.getFunction(qname.module.getOrElse(module), qname.name)
          if (function.isEmpty || function.get._2.argTypes.size != args.size) {
            val typeConstr = table.getConstructor(qname.module.getOrElse(module), qname.name)
            if (typeConstr.isEmpty || typeConstr.get._2.argTypes.size != args.size) {
              fatal(s"Function or constructor $qname is never defined or has the wrog number of args", expr.position)
            } else {
              S.Call(typeConstr.get._1, args.map(transformExpr(_)))
            }
          } else {
            S.Call(function.get._1, args.map(transformExpr(_)))
          }

        case N.Sequence(e1, e2) =>
          S.Sequence(transformExpr(e1), transformExpr(e2))

        case N.Let(paramDef, value, body) =>
          if (locals.contains(paramDef.name)) {
            fatal("Variable " + paramDef.name + " already defined")
          } else {
            if(params.contains(paramDef.name)){
              warning("Variable " + paramDef.name + " already defined")
            }
            val freshId = Identifier.fresh(paramDef.name)
            val newParamDef = (freshId, transformType(paramDef.tt, module))
            val newLocal = (paramDef.name, freshId)
            S.Let(S.ParamDef(newParamDef._1, S.TypeTree(newParamDef._2)), transformExpr(value), transformExpr(body)(module, (params, locals + newLocal)))
          }

        case N.Ite(cond, thenn, elze) =>
          S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))

        case N.Error(msg) =>
          S.Error(transformExpr(msg))

        case _ =>
          fatal("Unknown expression", expr)
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table, p)

  }
}
