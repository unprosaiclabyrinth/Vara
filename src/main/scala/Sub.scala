object Sub:
  def apply(l: Expr, r: Expr): Expr =
    (l + Neg(r)).simplify
    
  def unapply(e: Expr): Option[(Expr, Expr)] = e match
    case Add(l, Neg(r)) => Some((l, r))
    case _ => None
