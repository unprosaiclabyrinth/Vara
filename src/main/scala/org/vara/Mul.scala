package org.vara

import java.text.DecimalFormat

final case class Mul(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc *# e.eval)

  override def toString: String =
    if terms.exists {
      case Pow(_, Const(v)) if v < 0 => true
      case _ => false
    } then
      val (numr, denom) = terms.foldLeft(("", ""))((acc, e) => e match
        case Const(v) => (acc._1 + {
          val df = new DecimalFormat("#.####")
          if v == -1D then "-"
          else if v.isWhole then v.toInt.toString
          else "(" + df.format(v) + ")"
        }, acc._2)
        case Pow(base, Const(v)) if v < 0 =>
          if v == -1 then (acc._1, acc._2 + base.toString)
          else (acc._1, acc._2 + Pow(base, Const(-v)).toString)
        case _ => (acc._1 + e.toString, acc._2)
      )
      (if numr.startsWith("-") then "-" else "") + "\\frac{" + (if numr.isEmpty then "1" else numr.stripPrefix("-")) + "}{" + denom + "}"
    else
      terms.foldLeft("")((acc, e) => e match
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
