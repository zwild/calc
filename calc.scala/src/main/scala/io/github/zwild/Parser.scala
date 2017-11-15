package io.github.zwild

import scala.util.parsing.combinator.RegexParsers

// expr ::= term \{"+" term | "-" term\}.
// term ::= factor \{"*" factor | "/" factor\}.
// factor ::= number | "(" expr ")".
object CalcParsers extends RegexParsers {
  sealed trait Parens
  case object LParen extends Parens
  case object RParen extends Parens

  def lParen: Parser[Parens] = "(" ^^ { _ => LParen }
  def rParen: Parser[Parens] = ")" ^^ { _ => RParen }

  def number: Parser[Expr] = """\d+(\.\d*)?""".r ^^ { x => NumberExp(x.toDouble) }
  def variable: Parser[Expr] = """[a-zA-Z_]\w*""".r ^^ { VarExp(_) }

  def assignment: Parser[Expr] = variable ~ ":=" ~ numberExpr ^^ {
    case VarExp(v) ~ ":=" ~ e => AssignExp(v, e)
  }

  def function: Parser[Expr] = variable ~ lParen ~ exprAll ~ rParen ^^ {
    case VarExp(f) ~ lParen ~ e ~ rParen => FunExp(f, e)
  }

  def factor: Parser[Expr] = number | variable | "(" ~> numberExpr <~ ")"

  def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => OpExp(x, Times, y)
      case (x, "/" ~ y) => OpExp(x, Div, y)
    }
  }

  def numberExpr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "+" ~ y) => OpExp(x, Plus, y)
      case (x, "-" ~ y) => OpExp(x, Minus, y)
    }
  }

  def exprAll: Parser[Expr] = assignment | function | numberExpr | variable

  def apply(s: String): Either[CalcError, Expr] = parseAll(exprAll, s) match {
    case Success(expr, _) => Right(expr)
    case f: NoSuccess => println(f); Left(ParseError)
  }
}
