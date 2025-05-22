implicit class Variable(val name: String) extends Expr:
  override def eval(using env: Env): Expr = this
