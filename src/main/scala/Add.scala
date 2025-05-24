final case class Add(terms: Expr*) extends Expr:
  override def eval(using env: Env): Expr =
    terms.tail.foldLeft(terms.head.eval)((acc, e) => acc +~ e.eval)
  
  override def equals(that: Any): Boolean = that match
    case Add(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
