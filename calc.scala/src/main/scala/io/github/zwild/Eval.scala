package io.github.zwild

object Eval extends Env {
  private type Result = Either[CalcError, Double]

  def eval(exp: Expr): Result = exp match {
    case NumberExp(n) => Right(n)
    case OpExp(e1, op, e2) => evalOpExp(e1, op, e2)
    case VarExp(v) => evalVarExp(v)
    case AssignExp(v, e) => evalAssignExp(v, e)
    case FunExp(f, arg) => evalFunExp(f, arg)
  }

  private def evalOpExp(e1: Expr, op: Op, e2: Expr): Result = {
    def resultOp(x: Result, y: Result, op: (Double, Double) => Double): Result =
      if (x.isRight && y.isRight)
        for {
          x1 <- x
          y1 <- y
        } yield op(x1, y1)
      else if (x.isLeft) x
      else y

    val r1 = eval(e1)
    val r2 = eval(e2)

    op match {
      case Plus => resultOp(r1, r2, (_ + _ ))
      case Minus => resultOp(r1, r2, (_ - _))
      case Times => resultOp(r1, r2, (_ * _))
      case Div => resultOp(r1, r2, (_ / _))
    }
  }

  private def evalVarExp(v: String): Result = env.get(v) match {
    case Some(d) => Right(d)
    case None => Left(VariableNotFound(v))
  }

  private def evalAssignExp(v: String, e: Expr): Result =
    eval(e).map {
      d => env += (v -> d)
      env.get(v).get
    }

  private def evalFunExp(f: String, arg: Expr): Result = funEnv.get(f) match {
    case Some(fn) => eval(arg).map(d => fn(d))
    case None => Left(FunctionNotFound(f))
  }
}
