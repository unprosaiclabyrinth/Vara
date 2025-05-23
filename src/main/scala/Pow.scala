final case class Pow(base: Expr, index: Expr) extends Expr:
  def apply(base: Expr, index: Expr): Expr = base match
    case Pow(u, v) => Pow(u, Mul2(v, index))
    case _ => Pow(base, index)

  override def eval(using env: Env): Expr =
    base.eval(using env) ~: index.eval(using env)
