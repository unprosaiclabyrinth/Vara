import scala.math.pow

final case class Pow(base: Expr, index: Expr) extends Expr:
  override def eval(using env: Env): Expr =
    base.eval(using env).simplify^index.eval(using env).simplify
    
  override def simplify: Expr = (base.simplify, index.simplify) match
    case (Const(b), Const(i)) => Const(pow(b, i))
    case (l @ Pow(a, b), r) => Pow(l, Mul(b, r))
    case (l, r) => Pow(l, r)
