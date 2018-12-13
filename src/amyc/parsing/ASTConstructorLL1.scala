package amyc
package parsing

import grammarcomp.parsing._
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule
import amyc.utils.Positioned

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  override def constructDef0(pTree: NodeOrLeaf[Token]): NominalTreeModule.ClassOrFunDef = {
    pTree match {
      case Node('AbstractClassDef ::= _, List(Leaf(abs), _, name)) =>
        AbstractClassDef(constructName(name)._1).setPos(abs)
      case Node('CaseClassDef ::= _, List(Leaf(cse), _, name, _, params, _, _, parent)) =>
        CaseClassDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true).map(_.tt),
          constructName(parent)._1
        ).setPos(cse)
      case Node('FunDef ::= _, List(Node('OptDoc ::= List(DOCSENT), List(Leaf(DOC(text)))), Leaf(df), name, _, params, _, _, retType, _, _, body, _)) =>
        FunDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true),
          constructType(retType),
          constructExpr(body),
          Option(text)
        ).setPos(df)
      case Node('FunDef ::= _, List(Node('OptDoc ::= _, List(_)), Leaf(df), name, _, params, _, _, retType, _, _, body, _)) =>
        FunDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true),
          constructType(retType),
          constructExpr(body),
          Option.empty
        ).setPos(df)
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('OptExpr ::= _, List(expr)) =>
        constructExpr(expr)

      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExpr(value), constructExpr(body)).setPos(vt)

      case Node('Expr ::= _, List(exprMatch, Node('ExprValOpt ::= (SEMICOLON() :: _), List(_, e)))) =>
        val expr1 = constructExpr(exprMatch)
        val expr2 = constructExpr(e)
        Sequence(expr1, expr2).setPos(expr1)

      case Node('Expr ::= _, List(exprMatch, Node('ExprValOpt ::= _, List()))) =>
        constructExpr(exprMatch)

      case Node('ExprMatch ::= _, List(exprOR, Node('ExprMatchOpt ::= (MATCH() :: _), List(_, _, cases, _)))) =>
        val scrut = constructExpr(exprOR)
        Match(scrut, constructCaseList(cases, constructCase))

      case Node('ExprMatch ::= _, List(exprOR, Node('ExprMatchOpt ::= _, List()))) =>
        constructExpr(exprOR)

      case Node('ExprOR ::= _, List(exprAND, exprOR)) =>
        val pe = constructExpr(exprAND)
        constructOpExpr(pe, exprOR)

      case Node('ExprAND ::= _, List(exprEQUALS, exprAND)) =>
        val pe = constructExpr(exprEQUALS)
        constructOpExpr(pe, exprAND)

      case Node('ExprEQUALS ::= _, List(exprLESS, exprEQUALS)) =>
        val pe = constructExpr(exprLESS)
        constructOpExpr(pe, exprEQUALS)

      case Node('ExprLESS ::= _, List(exprFact1, exprLESS)) =>
        val pe = constructExpr(exprFact1)
        constructOpExpr(pe, exprLESS)

      case Node('ExprFact1 ::= _, List(exprFact2, exprFact1)) =>
        val pe = constructExpr(exprFact2)
        constructOpExpr(pe, exprFact1)

      case Node('ExprFact2 ::= _, List(unary, exprFact2)) =>
        val pe = constructExpr(unary)
        constructOpExpr(pe, exprFact2)

      case Node('Unary ::= List(BANG(), _), List(Leaf(bt), e)) =>
        Not(constructExpr(e)).setPos(bt)
      case Node('Unary ::= List(MINUS(), _), List(Leaf(mt), e)) =>
        Neg(constructExpr(e)).setPos(mt)
      case Node('Unary ::= _, List(atomicExpr)) =>
        constructExpr(atomicExpr)

      case Node('Atomic ::= (IF() :: _), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)

      case Node('Atomic ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)

        // id(args)
      case Node('Atomic ::= List('Id, 'OptFunc), List(id, Node('OptFunc ::= (LPAREN() :: _), List(_, as, _)))) =>
        val (name, pos) = constructName(id)
        val (qname, pos1) = (QualifiedName(None, name), pos)
        val args = constructList(as, constructExpr, hasComma = true)
        Call(qname, args).setPos(pos1)

      //id.id(args)
      case Node('Atomic ::= List('Id, 'OptFunc), List(module, Node('OptFunc ::= (DOT() :: _), List(_, name, _, as, _)))) =>
        val (module1, pos) = constructName(module)
        val (name1, _) = constructName(name)
        val (qname, pos1) = (QualifiedName(Some(module1), name1), pos)
        val args = constructList(as, constructExpr, hasComma = true)
        Call(qname, args).setPos(pos1)

      //id
      case Node('Atomic ::= List('Id, _), List(id, Node('OptFunc ::= _, List()))) =>
        val (name, pos) = constructName(id)
        Variable(name).setPos(pos)

      case Node('Atomic ::= List(LPAREN(), 'OptExpr, RPAREN()), List(Leaf(lp), expr, _)) =>
        constructOption(expr, constructExpr).getOrElse(UnitLiteral().setPos(lp))

      case Node('Atomic ::= List('Literal), List(lit)) =>
        constructLiteral(lit)
    }
  }

  //overrided because I deletedd the '()' in 'Literal
  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('Literal ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('Literal ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('Literal ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('Literal ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }

  //overrided because I deleted the '()' in literal and had to replace where literal was previously used
  //and because I modifed the grammar for 'Pattern
  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= _, List(Node('QName ::= _, List(id, Node('QNameOpt ::= _, List()))), Node('PatternOpt ::= _, List()))) =>
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
      case Node('Pattern ::= _, List(qn, Node('PatternOpt ::= _, List(_, patts, _)))) =>
        val (qname, pos) = constructQname(qn)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(qname, patterns).setPos(pos)
      case Node('Pattern ::= _, List(Leaf(lp@LPAREN()), Leaf(RPAREN()))) =>
        LiteralPattern(UnitLiteral().setPos(lp)).setPos(lp)
    }
  }
  //overrided because I modified the grammar for QName
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id, Node('QNameOpt ::= _, List()))) =>
        val (name, pos) = constructName(id)
        (QualifiedName(None, name), pos)
      case Node('QName ::= _, List(mod, Node('QNameOpt ::= _, List(_, nm)))) =>
        val (module, pos) = constructName(mod)
        val (name, _) = constructName(nm)
        (QualifiedName(Some(module), name), pos)
    }
  }

  //overrided because the op that are given are Leaf() and not Node(Leaf()) anymore
  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Leaf(t) =>
        tokenToExpr(t)
    }
  }

  //Special method for parsing 'Cases since I've modified the grammar, adapted from constructList1
  def constructCaseList[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List(t, Node('CaseOpt ::= _, List()))) => List(constructor(t))
      case Node(_, List(t, Node('CaseOpt ::= _, List(ts)))) =>
        constructor(t) :: constructCaseList(ts, constructor, hasComma)
    }
  }


  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('ExprOROpt, 'ExprANDOpt, 'ExprEQUALSOpt, 'ExprLESSOpt, 'ExprFact1Opt, 'ExprFact2Opt) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

}

