object Mul2:
  private def flattenMul(e: Expr): List[Expr] = e match
    case Mul(first, rest*) => rest.foldLeft(List(first))((acc, e) => acc ++ flattenMul(e))
    case _ => List(e)
    
  def apply(left: Expr, right: Expr): Expr =
    val terms: List[Expr] = flattenMul(left) ++ flattenMul(right)
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
      val varTerms: List[Expr] =
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
        }(Add2(_, _)).toList.map(Pow.apply)
      varTerms match
        case Nil => Const(constProd)
        case _ =>
          if constProd == 1D then Mul(varTerms*)
          else Mul(Const(constProd) :: varTerms*)
