final case class Const(value: Double) extends Expr:
  override def eval(using env: Env): Expr = this
  
  override def equals(that: Any): Boolean = that match
    case Const(thatValue) => value == thatValue
    case _ => false
    
  override def hashCode: Int = value.hashCode
