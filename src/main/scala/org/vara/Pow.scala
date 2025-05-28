package org.vara

final case class Pow(base: VaraExpr, index: VaraExpr) extends VaraExpr:
  override def eval(using env: VaraEnv): VaraExpr = base.eval #: index.eval

  override def toString: String = index match
    case Const(v) if v == -1 => s"\\frac{1}{$base}"
    case Const(v) if v < 0 => s"\\frac{1}{{$base}^{${-v}}}"
    case Mul(Const(v), e*) if v < 0 => s"\\frac{1}{{$base}^{${Mul(Const(-v) +: e*)}}"
    case _ => s"{$base}^{$index}"

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
    case _ => new Pow(base, index)
