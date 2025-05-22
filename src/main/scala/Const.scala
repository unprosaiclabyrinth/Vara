final case class Const(value: Double) extends Expr:
  override def eval(using env: Env): Expr = this

  override def simplify: Expr = this
