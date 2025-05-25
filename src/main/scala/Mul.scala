final case class Mul(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc *~ e.eval)

  override def toString: String = terms.foldLeft("")((acc, e) => e match
    case Const(v) => acc + (if v.isWhole then v.toInt.toString else "("+v.toString+")")
    case _ => acc + s"($e)"
  )
  
  override def equals(that: Any): Boolean = that match
    case Mul(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
