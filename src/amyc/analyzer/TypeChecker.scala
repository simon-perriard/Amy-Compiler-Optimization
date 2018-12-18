package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.{Identifier, SymbolicTreeModule => S}

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(S.Program, SymbolTable), (S.Program, SymbolTable)] {

  def run(ctx: Context)(v: (S.Program, SymbolTable)): (S.Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private(id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]

      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {

      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)

        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)

        case StringLiteral(_) =>
          topLevelConstraint(StringType)

        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val commonType = TypeVariable.fresh()
          genConstraints(lhs, commonType) ++ genConstraints(rhs, commonType) ++ topLevelConstraint(BooleanType)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
          (List[Constraint], Map[Identifier, Type]) = {
            pat match {
              case WildcardPattern() =>
                (Nil, Map.empty) //No constraint

              case IdPattern(idPattern) =>
                (genConstraints(Variable(idPattern), scrutExpected)(env + (idPattern -> scrutExpected)), Map(idPattern -> scrutExpected))

              case LiteralPattern(lit) =>
                (genConstraints(lit, scrutExpected), Map.empty)

              case CaseClassPattern(constr, args) =>
                val typeConstraint = Constraint(table.getConstructor(constr).get.retType, scrutExpected, pat.position)
                val argsConstraints : List[(List[Constraint], Map[Identifier, Type])] = args.zip(table.getConstructor(constr).get.argTypes).map {
                  case (arg, t) => handlePattern(arg, t)
                }

                argsConstraints.foldLeft((List(typeConstraint), Map[Identifier, Type]())) {
                  case ((accList, accMap), (list, map)) =>
                    (accList ++ list, accMap ++ map)
                }
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            //Body Type must be of the return type
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv) ++ topLevelConstraint(expected)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Variable(id) =>
          topLevelConstraint(env(id))

        case Plus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Minus(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Times(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Div(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case Mod(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)

        case LessThan(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case LessEquals(lhs, rhs) =>
          genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)

        case And(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Or(lhs, rhs) =>
          genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)

        case Concat(lhs, rhs) =>
          genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)

        case Not(e) =>
          genConstraints(e, BooleanType) ++ topLevelConstraint(BooleanType)

        case Neg(e) =>
          genConstraints(e, IntType) ++ topLevelConstraint(IntType)

        case Call(qname, args) =>
          val function = table.getFunction(qname)
          if (function.isEmpty) {
            val typeConstr = table.getConstructor(qname).get
            val argsConstraint: List[Constraint] = args.zip(typeConstr.argTypes).foldLeft(List[Constraint]()) {
              case (acc, elem) =>
                acc ++ genConstraints(elem._1, elem._2)
            }
            argsConstraint ++ topLevelConstraint(typeConstr.retType)
          } else {
            val argsConstraint: List[Constraint] =
              args.zip(function.get.argTypes).foldLeft(List[Constraint]()) {
                case (acc, elem) =>
                  acc ++ genConstraints(elem._1, elem._2)
              }
            argsConstraint ++ topLevelConstraint(function.get.retType)
          }

        case Sequence(e1, e2) =>
          //Expected return type is the type of the last expr
          val e2Type = TypeVariable.fresh()
          genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, e2Type) ++ topLevelConstraint(expected)

        case Let(df, value, body) =>
          val bodyType = TypeVariable.fresh()
          //Expected return type is body type
          genConstraints(value, df.tt.tpe) ++ genConstraints(body, bodyType)(env + (df.name -> df.tt.tpe)) ++ topLevelConstraint(expected)

        case Ite(cond, thenn, elze) =>
          genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected) ++ topLevelConstraint(expected)

        case Error(msg) =>
          genConstraints(msg, StringType) ++ topLevelConstraint(expected)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          expected match {
            case TypeVariable(varTypeId) =>
              // replace all occurrences of this TypeVariable by the found Type
              // and continue constraint solving
              solveConstraints(subst_*(more, varTypeId, found))

            case _ =>
              //Types don't correspond
              if (found.toString != expected.toString) {
                error("Expected : " + expected + " but found : " + found, pos)
              } else {
                solveConstraints(more)
              }
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body, _) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }

}
