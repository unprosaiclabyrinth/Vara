package org.vara

import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

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

object VaraExpr:
  type VaraEnv = Map[String, VaraExpr]
  given emptyEnv: VaraEnv = Map.empty

  implicit inline def fromDouble(d: Double): VaraExpr = Const(d)

  implicit inline def fromInt(i: Int): VaraExpr = Const(i.toDouble)

  infix inline def e(inline s: String): VaraExpr = s

  infix inline def e(inline i: Int): VaraExpr = i

  infix inline def e(inline d: Double): VaraExpr = d

  /* put API */
  case class Put(bindings: (String, VaraExpr)*):
    infix def in(expr: VaraExpr): VaraExpr =
      val env = summon[VaraEnv]
      expr.eval(using env ++ bindings.toMap)

  infix def put(bindings: (String, VaraExpr)*): Put = Put(bindings *)

  /* replace API */
  case class ReplaceWith(old: VaraExpr, replacement: VaraExpr):
    infix def in(expr: VaraExpr): VaraExpr =
      if !expr.contains(old) then expr
      else if old == expr then replacement
      else expr match
        case Add(sum*) =>
          if old.isInstanceOf[Add] && old.asInstanceOf[Add].terms.forall(sum contains) then
            val subsum = old.asInstanceOf[Add].terms
            sum.filterNot(subsum contains).foldLeft(replacement)(
              (acc, term) => acc +# term
            )
          else if sum contains old then sum.filterNot(_ == old).foldLeft(replacement) {
            (acc, term) => acc +# term
          }
          else sum.foldLeft(e(0))((acc, term) => acc +# in(term))
        case Mul(prod*) =>
          if old.isInstanceOf[Mul] && old.asInstanceOf[Mul].terms.forall(prod contains) then
            val subprod = old.asInstanceOf[Mul].terms
            prod.filterNot(subprod contains).foldLeft(replacement)(
              (acc, term) => acc *# term
            )
          else if prod contains old then prod.filterNot(_ == old).foldLeft(replacement) {
            (acc, term) => acc *# term
          }
          else prod.foldLeft(e(1))((acc, term) => acc *# in(term))
        case Pow(a, b) => in(a) #: in(b)
        case _ => expr // old==expr is already checked

  case class Replace(old: VaraExpr):
    infix def withExpr(replacement: VaraExpr): ReplaceWith = ReplaceWith(old, replacement)

  infix def replace(old: VaraExpr): Replace = Replace(old)

  /** distribution API */
  case class Distribute():
    infix def product(prod: VaraExpr): DistributeProduct = DistributeProduct(prod)

    infix def exponent(exp: VaraExpr): DistributeExponent = DistributeExponent(exp)

  val distribute: Distribute = Distribute()

  case class DistributeProduct(prod: VaraExpr):
    infix def over(sum: VaraExpr): DistributeProductOver = DistributeProductOver(prod, sum)

  case class DistributeExponent(exp: VaraExpr):
    infix def over(prod: VaraExpr): DistributeExponentOver = DistributeExponentOver(exp, prod)

  case class DistributeProductOver(prod: VaraExpr, sum: VaraExpr):
    infix def in(e: VaraExpr): VaraExpr = sum match
      case Add(terms *) =>
        val distributed: VaraExpr = terms.foldLeft(Const(0).asInstanceOf[VaraExpr]) {
          case (acc, t) => acc +# (t *# prod)
        }
        replace (prod *# sum) withExpr distributed in e
      case _ => e

  case class DistributeExponentOver(exp: VaraExpr, prod: VaraExpr):
    infix def in(e: VaraExpr): VaraExpr = prod match
      case Mul(terms *) =>
        val distributed: VaraExpr = terms.foldLeft(Const(1).asInstanceOf[VaraExpr]) {
          case (acc, t) => acc *# (t #: exp)
        }
        replace (prod #: exp) withExpr distributed in e
