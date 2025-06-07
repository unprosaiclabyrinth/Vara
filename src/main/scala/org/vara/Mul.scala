package org.vara

import java.text.DecimalFormat
import VaraExpr.e

final case class Mul(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc *# e.eval)

  override def toString: String =
    if terms.exists {
      case Pow(_, Const(v)) if v < 0 => true
      case _ => false
    } then
      val (numr, notNumr) = terms.partition {
        case Pow(_, Const(v)) if v < 0 => false
        case _ => true
      }
      val denom = notNumr.map(d =>
        val e = d.asInstanceOf[Pow]
        val index = e.index.asInstanceOf[Const]
        if index.value == -1 then e.base else Pow(e.base, Const(-index.value))
      )
      val (sn, sd) = (numr.foldLeft(e(1D))((acc, n) => acc *# n).toString,
        denom.foldLeft(e(1D))((acc, d) => acc *# d).toString)
      val sign =
        if (sn.startsWith("-") && !sd.startsWith("-")) ||
          (!sn.startsWith("-") && sd.startsWith("-")) then "-"
        else ""
      sign + "\\frac{" + sn.stripPrefix("-") + "}{" + sd.stripPrefix("-") + "}"
    else
      terms.foldLeft("")((acc, e) => e match
        case Const(v) => acc + {
          val df5 = new DecimalFormat("#.#####")
          if v == -1D then "-"
          else if v.isWhole then v.toInt.toString
          else "(" + df5.format(v) + ")"
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
