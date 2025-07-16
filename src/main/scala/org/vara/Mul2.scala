package org.vara

object Mul2:
  private def flattenMul(e: VaraExpr): List[VaraExpr] = e match
    case Mul(first, rest*) => rest.foldLeft(flattenMul(first))((acc, e) => acc ++ flattenMul(e))
    case _ => List(e)

  def apply(left: VaraExpr, right: VaraExpr): VaraExpr =
    val terms: List[VaraExpr] = flattenMul(left) ++ flattenMul(right)
    val (consts, vars) = terms.partition {
      case Const(_) => true
      case _ => false
    }
    if consts contains Const(0D) then Const(0D)
    else
      val constProd: Double = consts.foldLeft(1D) {
        case (acc, Const(v)) => acc * v
        case (acc, _) => acc
      }
      val varTerms: List[VaraExpr] =
        vars.groupBy(identity).toList.map((expr, copies) =>
          copies.size match
            case 1 => expr
            case k => Pow(expr, Const(k))
        ).groupMapReduce {
          case Pow(b, i) => b
          case e => e
        }{
          case Pow(b, i) => i
          case _ => Const(1D)
        }(_ +# _).toList.map(Pow.apply)
          .filterNot(_.isInstanceOf[Const]) // the only Const possible is 1 so get rid
      varTerms match
        case Nil => Const(constProd)
        case h :: Nil =>
          if constProd == 1D then h
          else Mul(List(Const(constProd), h) *)
        case _ =>
          if constProd == 1D then Mul(varTerms *)
          else Mul(Const(constProd) :: varTerms *)
