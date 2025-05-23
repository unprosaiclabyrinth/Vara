object Neg:
  def apply(e: Expr): Expr = Const(-1D) ** e
