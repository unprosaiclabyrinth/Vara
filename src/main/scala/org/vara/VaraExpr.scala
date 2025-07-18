package org.vara

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}
import scala.math.abs

/* Expression AST */
trait VaraExpr:
  type VaraEnv = Map[String, VaraExpr]
  given emptyEnv: VaraEnv = Map.empty

  /** Evaluate an expression partially or fully from the given environment */
  def eval(using env: VaraEnv): VaraExpr

  /** Evaluate an expression fully to a value */
  def value(using env: VaraEnv): Try[Double] = eval match
    case Const(v) => Success(v)
    case other => Failure(new IllegalStateException(s"Unbound variables in: $other"))

  // Operators (according to correct precedence and assoc)
  infix def #:(that: VaraExpr): VaraExpr = Pow(that, this) // right associativity

  infix def *#(that: VaraExpr): VaraExpr = Mul2(this, that)

  infix def /#(that: VaraExpr): VaraExpr = Div(this, that)

  infix def +#(that: VaraExpr) : VaraExpr = Add2(this, that)

  infix def -#(that: VaraExpr): VaraExpr = Sub(this, that)

  infix def unary_- : VaraExpr = Neg(this)

  private def ast(e: VaraExpr, indent: String): String = e match
    case Add(h, t*) =>
      s"${indent}Add(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Mul(h, t*) =>
      s"${indent}Mul(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Pow(a, b) => s"${indent}Pow(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
    case Const(v) => s"${indent}Const(" + e.toString + ")"
    case _ => s"${indent}Var(" + e.toString + ")"

  def printAST(): Unit = println(ast(this, ""))

  infix def contains(e: VaraExpr): Boolean =
    this == e || {
      this match
        case Add(terms*) => e match
          case Add(subterms*) =>
            (terms exists (_.contains(e))) || (subterms forall (terms contains))
          case _ => terms exists (_.contains(e))
        case Mul(terms*) => e match
          case Mul(subterms*) =>
            (terms exists (_.contains(e))) || (subterms forall (terms contains))
          case _ => terms exists (_.contains(e))
        case Pow(a, b) => a.contains(e) || b.contains(e)
        case _ => false // this==e is already checked
    }

  infix def expanded: VaraExpr = VaraExpr.expand (this) in this

object VaraExpr:
  type VaraEnv = Map[String, VaraExpr]
  given emptyEnv: VaraEnv = Map.empty

  implicit inline def fromDouble(d: Double): VaraExpr = Const(d)

  implicit inline def fromInt(i: Int): VaraExpr = Const(i.toDouble)

  infix inline def e(inline s: String): VaraExpr = s

  infix inline def e(inline i: Int): VaraExpr = i

  infix inline def e(inline d: Double): VaraExpr = d

  /** `put` API */
  case class Put(bindings: (String, VaraExpr)*):
    infix def in(expr: VaraExpr): VaraExpr =
      val env = summon[VaraEnv]
      expr.eval(using env ++ bindings.toMap)

  infix def put(bindings: (String, VaraExpr)*): Put = Put(bindings *)

  /** `substitute` API */
  case class SubstituteFor(substitution: VaraExpr, old: VaraExpr):
    infix def in(expr: VaraExpr): VaraExpr =
      if !expr.contains(old) then expr
      else if old == expr then substitution
      else expr match
        case Add(sum*) =>
          if old.isInstanceOf[Add] && old.asInstanceOf[Add].terms.forall(sum contains) then
            val subsum = old.asInstanceOf[Add].terms
            sum.filterNot(subsum contains).foldLeft(substitution)(
              (acc, term) => acc +# term
            )
          else if sum contains old then sum.filterNot(_ == old).foldLeft(substitution) {
            (acc, term) => acc +# term
          }
          else sum.foldLeft(e(0))((acc, term) => acc +# in(term))
        case Mul(prod*) =>
          if old.isInstanceOf[Mul] && old.asInstanceOf[Mul].terms.forall(prod contains) then
            val subprod = old.asInstanceOf[Mul].terms
            prod.filterNot(subprod contains).foldLeft(substitution)(
              (acc, term) => acc *# term
            )
          else if prod contains old then prod.filterNot(_ == old).foldLeft(substitution) {
            (acc, term) => acc *# term
          }
          else prod.foldLeft(e(1))((acc, term) => acc *# in(term))
        case Pow(a, b) => in(a) #: in(b)
        case _ => expr // old==expr is already checked

  case class Substitute(substitution: VaraExpr):
    infix def forExpr(old: VaraExpr): SubstituteFor = SubstituteFor(substitution, old)

  infix def substitute(substitution: VaraExpr): Substitute = Substitute(substitution)

  /** `distribute` API */
  case class Distribute(e1: VaraExpr):
    infix def over(e2: VaraExpr): DistributeOver = DistributeOver(e1, e2)

  case class DistributeOver(e1: VaraExpr, e2: VaraExpr):
    infix def in(e: VaraExpr): VaraExpr = e2 match
      case Add(terms *) =>
        val distributed: VaraExpr = terms.foldLeft(Const(0).asInstanceOf[VaraExpr]) {
          case (acc, t) => acc +# (e1 *# t)
        }
        substitute (distributed) forExpr e1 *# e2 in e
      case Mul(terms *) =>
        val distributed: VaraExpr = terms.foldLeft(Const(1).asInstanceOf[VaraExpr]) {
          case (acc, t) => acc *# (t #: e1)
        }
        substitute (distributed) forExpr e2 #: e1 in e
      case _ => e

  infix def distribute(e1: VaraExpr): Distribute = Distribute(e1)

  /** `expand` API */
  case class Expand(e: VaraExpr):
    private def expand2(sum1: Add, sum2: Add): Add =
      val e = sum1 *# sum2
      val d = distribute (sum1) over sum2 in e
      sum2.terms.foldLeft(d) {
        (acc, t) => distribute (t) over sum1 in acc
      }.asInstanceOf[Add]

    @tailrec
    private def expandN(sums: Add*): VaraExpr =
      require(sums.length >= 2)
      val d = expand2(sums.head, sums.tail.head)
      if sums.length == 2 then d
      else expandN(d +: sums.tail.tail *)

    infix def in(expr: VaraExpr): VaraExpr = e match
      case Mul(prod *) =>
        val sums: Seq[Add] = prod.filter(_.isInstanceOf[Add]).map(_.asInstanceOf[Add])
        if sums.length <= 1 then expr
        else
          val expansion = expandN(sums *)
          val factors = prod.filterNot(_.isInstanceOf[Mul | Add])
          substitute (
            distribute (Mul(factors *)) over expansion in Mul(expansion +: factors *)
          ) forExpr e in expr
      case Pow(base, Const(v)) =>
        if !v.isWhole then expr
        else
          val prod = Mul(List.fill(abs(v.toInt))(base) *)
          val expansion = expand (prod) in prod
          if v < 0 then substitute (1 /# expansion) forExpr e in expr
          else substitute (expansion) forExpr e in expr
      case _ => expr

  infix def expand(e: VaraExpr): Expand = Expand(e)
