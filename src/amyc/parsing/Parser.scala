package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens.{LPAREN, _}

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
      'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
      VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
      IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
      'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
      ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
      LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
      AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // TODO: Write a grammar that implements the correct syntax of Amy and is LL1.
  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id ~ 'QNameOpt,
    'QNameOpt ::= epsilon() | DOT() ~ 'Id,

    'Expr ::= 'ExprMatch ~ 'ExprValOpt | VAL() ~ 'Param ~ EQSIGN() ~ 'ExprOR ~ SEMICOLON() ~ 'OptExpr,
    'ExprValOpt ::= SEMICOLON() ~ 'OptExpr | epsilon(),

    'ExprMatch ::= 'ExprOR ~ 'ExprMatchOpt,
    'ExprMatchOpt ::= MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | epsilon(),

    'ExprOR ::= 'ExprAND ~ 'ExprOROpt,
    'ExprOROpt ::= OR() ~ 'ExprOR | epsilon(),

    'ExprAND ::= 'ExprEQUALS ~ 'ExprANDOpt,
    'ExprANDOpt ::= AND() ~ 'ExprAND | epsilon(),

    'ExprEQUALS ::= 'ExprLESS ~ 'ExprEQUALSOpt,
    'ExprEQUALSOpt ::= EQUALS() ~ 'ExprEQUALS | epsilon(),

    'ExprLESS ::= 'ExprFact1 ~ 'ExprLESSOpt,
    'ExprLESSOpt ::= LESSTHAN() ~'ExprLESS | LESSEQUALS() ~'ExprLESS | epsilon(),

    'ExprFact1 ::= 'ExprFact2 ~ 'ExprFact1Opt,
    'ExprFact1Opt ::= PLUS() ~ 'ExprFact1 | MINUS() ~ 'ExprFact1 | CONCAT() ~ 'ExprFact1 | epsilon(),

    'ExprFact2 ::= 'Unary ~ 'ExprFact2Opt,
    'ExprFact2Opt ::= TIMES() ~ 'ExprFact2 | DIV() ~ 'ExprFact2 | MOD() ~ 'ExprFact2 | epsilon(),

    'Unary ::= MINUS() ~ 'Atomic | BANG() ~ 'Unary | 'Atomic,

    'Atomic ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
      ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() | 'Literal | 'Id ~ 'OptFunc /*cannot use QName otherwise id.id is permitted in Expr*/| LPAREN() ~ 'OptExpr ~ RPAREN(), // (OptExpr) instead of (Expr) to compensate the loss of () in literal

    'OptFunc ::= epsilon() | DOT() ~ 'Id ~ LPAREN() ~ 'Args ~ RPAREN() | LPAREN() ~ 'Args ~ RPAREN(),

    'Literal ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'Cases ::= 'Case ~ 'CaseOpt,
    'CaseOpt ::= 'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'QName ~ 'PatternOpt | LPAREN() ~ RPAREN(), // () to compensate the loss of () in Literal
    'PatternOpt ::= LPAREN() ~ 'Patterns ~ RPAREN() | epsilon(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))


  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    // TODO: Switch to LL1 when you are ready
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
      // info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}