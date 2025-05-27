package org.vara

import java.text.DecimalFormat

final case class Mul(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc *# e.eval)

  override def toString: String = terms.foldLeft("")((acc, e) => e match
    case Const(v) => acc + {
      val df = new DecimalFormat("#.####")
      if v == -1D then "-"
      else if v.isWhole then v.toInt.toString
      else "(" + df.format(v) + ")"
    }
    case _: Pow => acc + e.toString
    case _ => acc + {
      val s = e.toString
      if s.length == 1 then s
      else "(" + s + ")"
    }
  )
  
  override def equals(that: Any): Boolean = that match
    case Mul(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
