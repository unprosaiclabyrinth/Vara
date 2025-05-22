final case class Neg(e: Expr) extends Expr:
  override def eval(using env: Env): Expr = -e.eval(using env).simplify
  
  override def simplify: Expr = e match
    case Const(v) => Const(-v)
    case _ => Neg(e.simplify)
