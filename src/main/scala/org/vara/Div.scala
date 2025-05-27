package org.vara

import VaraExpr.fromInt

object Div:
  def apply(numr: VaraExpr, denom: VaraExpr): VaraExpr = numr *# denom #: -1
