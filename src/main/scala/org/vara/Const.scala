package org.vara

import java.text.DecimalFormat

final case class Const(value: Double) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr = this

  override def toString: String = {
    val df5 = new DecimalFormat("#.##5##")
    s"${if value.isWhole then value.toInt else df5.format(value)}"
  }

  override def equals(that: Any): Boolean = that match
    case Const(thatValue) => value == thatValue
    case n: java.lang.Number => value == n.doubleValue
    case _ => false

  override def hashCode: Int = value.hashCode
