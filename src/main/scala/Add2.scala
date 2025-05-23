object Add2:
  private def flattenAdd(e: Expr): List[Expr] = e match
    case Add(first, rest*) => rest.foldLeft(List(first))((acc, e) => acc ++ flattenAdd(e))
    case _ => List(e)

  def apply(left: Expr, right: Expr): Expr =
    val terms: List[Expr] = flattenAdd(left) ++ flattenAdd(right)
    val (consts, vars) = terms.partition {
      case Const(_) => true
      case _ => false
    }
    val constSum: Double = consts.foldLeft(0D) {
      case (acc, Const(d)) => acc + d
      case (acc, _) => acc
    }
    val varTerms: List[Expr] =
      vars.groupBy(identity).toList.map((expr, copies) =>
        copies.size match
          case 1 => expr
          case k => Const(k.toDouble) ** expr
      ).groupMapReduce {
        case Mul(Const(k), e*) => e
        case e => Seq(e)
      }{
        case Mul(Const(k), e*) => k
        case _ => 1D
      }(_ + _).toList.map((e, k) =>
        val seq = Const(k) +: e
        seq.tail.foldLeft(seq.head)((acc, e) => acc ** e)
      ).filterNot(_.isInstanceOf[Const]) // the only Const possible is 0 so get rid
    varTerms match
      case Nil => Const(constSum)
      case _ =>
        if constSum == 0D then Add(varTerms*)
        else Add(varTerms :+ Const(constSum)*)
