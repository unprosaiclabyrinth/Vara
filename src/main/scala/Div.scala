import Expr.fromInt

object Div:
  def apply(numr: Expr, denom: Expr): Expr = numr ** denom |: -1
