final case class Pow(base: Expr, index: Expr) extends Expr:
  def apply(base: Expr, index: Expr): Expr = (base, index) match
    case (_, Const(0D)) | (Const(1D), _) => Const(1D)
    case (b, Const(1D)) => b
    case (Const(0D), _) => Const(0D)
    case (Const(b), Const(i)) => Const(math.pow(b, i))
    case (Pow(u, v), _) => Pow(u, Mul2(v, index))
    case _ => Pow(base, index)

  override def eval(using env: Env): Expr = base.eval |: index.eval

  override def equals(that: Any): Boolean = that match
    case Pow(u, v) => base == u && index == v
    case _ => false

  override def hashCode: Int = (base, index).hashCode
