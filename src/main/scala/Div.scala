object Div:
  def apply(left: Expr, right: Expr): Expr =
    Mul(left, Pow(right, Const(-1.0))).simplify

  def unapply(expr: Expr): Option[(Expr, Expr)] = expr match
    case Div(l, r) => Some((l, r))
    case _ => None
