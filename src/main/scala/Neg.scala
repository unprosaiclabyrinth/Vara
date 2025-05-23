object Neg:
  def apply(e: Expr): Expr = Mul2(Const(-1D), e)
