object Div:
  def apply(numr: Expr, denom: Expr): Expr = Mul2(numr, Pow(denom, -1))
