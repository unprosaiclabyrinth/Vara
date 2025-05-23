final case class Mul(terms: Expr*) extends Expr:
  override def eval(using env: Env): Expr =
    Mul(terms.map(_.eval(using env))*)

  override def equals(that: Any): Boolean = that match
    case Mul(those*) => those.toSet == terms.toSet
    case _ => false

  override def hashCode: Int = terms.toSet.hashCode
