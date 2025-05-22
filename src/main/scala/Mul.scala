final case class Mul(left: Expr, right: Expr) extends Expr:
  override def eval(using env: Env): Expr =
    left.eval(using env) * right.eval(using env)
