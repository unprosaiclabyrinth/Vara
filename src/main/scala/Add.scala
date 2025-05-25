final case class Add(terms: VaraExpr*) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc +~ e.eval)

  override def toString: String = terms.mkString(" + ").replace("+ -", "- ")

  override def equals(that: Any): Boolean = that match
    case Add(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
