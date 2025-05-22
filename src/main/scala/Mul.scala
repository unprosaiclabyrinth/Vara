final case class Mul(left: Expr, right: Expr) extends Expr:
  override def eval(using env: Env): Expr = ???

  override def simplify: Expr = ???
 