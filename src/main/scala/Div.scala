final case class Div(numr: Expr, denom: Expr) extends Expr:
  override def eval(using env: Env): Expr =
    numr.eval(using env) / denom.eval(using env)
