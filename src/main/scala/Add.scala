final case class Add(left: Expr, right: Expr) extends Expr:
  /** Flatten about the add operation */
  private def flattenAdd(e: Expr): List[Expr] = e match
    case Add(l, r) => flattenAdd(l) ++ flattenAdd(r)
    case _ => List(e)
  
  override def eval(using env: Env): Expr =
    left.eval(using env).simplify + right.eval(using env).simplify
    
  override def simplify: Expr =
    // gather all terms, simplified
    val terms = flattenAdd(this).map(_.simplify)
    // pull out the numeric constants
    val (consts, vars) = terms.partition(_.isInstanceOf[Const])
    val constSum = consts.foldLeft(0.0) {
      case (acc, Const(v)) => acc + v
      case (acc, _) => acc
    }
    // group identical non-constant terms, count repeats
    val grouped: List[Expr] = vars
      .groupBy(identity).toList.map((expr, copies) =>
        copies.size match
          case 1 => expr
          case k => Mul(Const(k.toDouble), expr).simplify
      )
    {
      if constSum != 0.0 then Const(constSum) :: grouped
      else grouped
    } match
      case Nil => Const(0)
      case h :: Nil => h
      case h :: t => t.foldLeft(h)((acc, term) => Add(acc, term))
      