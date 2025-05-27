package org.vara

import VaraExpr.fromInt

object Neg:
  def apply(e: VaraExpr): VaraExpr = -1 *# e
