package org.vara

object Sub:
  def apply(left: VaraExpr, right: VaraExpr): VaraExpr = left +# -right
