package org.vara

final case class Pow(base: VaraExpr, index: VaraExpr) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr = base.eval #: index.eval

  override def toString: String =
    val baseStr = base.toString
    val indexStr = index.toString
    if baseStr.length == 1 && indexStr.length == 1 then baseStr + "^" + indexStr
    else if baseStr.length == 1 then "{" + baseStr + "}^{" + indexStr + "}"
    else "{(" + baseStr + ")}^{" + indexStr + "}"

  override def equals(that: Any): Boolean = that match
    case Pow(u, v) => base == u && index == v
    case _ => false

  override def hashCode: Int = (base, index).hashCode

object Pow:
  def apply(base: VaraExpr, index: VaraExpr): VaraExpr = (base, index) match
    case (Const(0D), Const(-1D)) => throw new RuntimeException("*** Division by zero.")
    case (Const(0D), _) => Const(0D)
    case (_, Const(0D)) | (Const(1D), _) => Const(1D)
    case (b, Const(1D)) => b
    case (Const(b), Const(i)) => Const(math.pow(b, i))
    case (Pow(u, v), _) => new Pow(u, v*#index)
    case (Mul(prod *), Const(-1)) => prod.foldLeft(Const(1).asInstanceOf[VaraExpr]){
      (acc, term) => acc *# new Pow(term, index)
    }
    case _ => new Pow(base, index)
