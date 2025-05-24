final case class Mul(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: Env): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc *~ e.eval)
  
  override def equals(that: Any): Boolean = that match
    case Mul(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
