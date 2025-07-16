package org.vara

object Add2:
  private def flattenAdd(e: VaraExpr): List[VaraExpr] = e match
    case Add(first, rest*) => rest.foldLeft(flattenAdd(first))((acc, e) => acc ++ flattenAdd(e))
    case _ => List(e)

  def apply(left: VaraExpr, right: VaraExpr): VaraExpr =
    val terms: List[VaraExpr] = flattenAdd(left) ++ flattenAdd(right)
    val (consts, vars) = terms.partition {
      case Const(_) => true
      case _ => false
    }
    val constSum: Double = consts.foldLeft(0D) {
      case (acc, Const(d)) => acc + d
      case (acc, _) => acc
    }
    val varTerms: List[VaraExpr] =
      vars.groupBy(identity).toList.map((expr, copies) =>
        copies.size match
          case 1 => expr
          case k => Const(k.toDouble) *# expr
      ).groupMapReduce {
        case Mul(Const(k), e*) => Mul(e*)
        case Mul(e*) => Mul(e*)
        case e => Mul(e)
      }{
        case Mul(Const(k), e*) => k
        case _ => 1D
      }(_ + _).toList.map((e, k) =>
        val seq = Const(k) +: e.terms
        seq.tail.foldLeft(seq.head)((acc, e) => acc *# e)
      ).filterNot(_.isInstanceOf[Const]) // the only Const possible is 0 so get rid
    varTerms match
      case Nil => Const(constSum)
      case h :: Nil =>
        if constSum == 0D then h
        else Add(List(h, Const(constSum)) *)
      case _ =>
        if constSum == 0D then Add(varTerms *)
        else Add(varTerms :+ Const(constSum) *)
