final case class Pow(base: Expr, index: Expr) extends Expr:
  override def eval(using env: Env): Expr =
    base.eval(using env) ^ index.eval(using env)
