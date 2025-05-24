import Expr.fromInt

object Neg:
  def apply(e: Expr): Expr = -1 *~ e
