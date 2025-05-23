final case class Add(terms: Expr*) extends Expr:
  override def eval(using env: Env): Expr =
    Add(terms.map(_.eval(using env))*)

  override def equals(that: Any): Boolean = that match
    case Add(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
