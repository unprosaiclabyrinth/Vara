final case class Neg(e: Expr) extends Expr:
  override def eval(using env: Env): Expr = -e.eval(using env)
