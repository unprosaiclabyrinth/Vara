object Sub:
  def apply(left: Expr, right: Expr): Expr = left +~ -right
