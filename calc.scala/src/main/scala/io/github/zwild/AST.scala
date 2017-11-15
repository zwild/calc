package io.github.zwild

sealed trait Expr
final case class NumberExp(num: Double) extends Expr {
  override def toString = num.toString
}
final case class OpExp(expr1: Expr, op: Op, expr2: Expr) extends Expr {
  override def toString = "(" + expr1 + op.toString + expr2 + ")"
}
final case class VarExp(v: String) extends Expr {
  override def toString = v
}
final case class AssignExp(v: String, expr: Expr) extends Expr {
  override def toString = v + ":=" + expr
}
final case class FunExp(f: String, arg: Expr) extends Expr {
  override def toString = f + "(" + arg + ")"
}

sealed trait Op
final case object Plus extends Op {
  override def toString = "+"
}
final case object Minus extends Op {
  override def toString = "-"
}
final case object Times extends Op {
  override def toString = "*"
}
final case object Div extends Op {
  override def toString = "/"
}

sealed trait CalcError
final case object ParseError extends CalcError
final case class VariableNotFound(v: String) extends CalcError
final case class FunctionNotFound(f: String) extends CalcError
