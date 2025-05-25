import java.text.DecimalFormat

final case class Const(value: Double) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr = this

  override def toString: String = {
    val df = new DecimalFormat("#.####")
    s"${if value.isWhole then value.toInt else df.format(value)}"
  }

  override def equals(that: Any): Boolean = that match
    case Const(thatValue) => value == thatValue
    case n: java.lang.Number => value == n.doubleValue
    case _ => false

  override def hashCode: Int = value.hashCode
