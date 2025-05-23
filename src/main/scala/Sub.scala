object Sub:
  def apply(left: Expr, right: Expr): Expr = Add2(left, Neg(right))
