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

object VaraExpr:
  type VaraEnv = Map[String, VaraExpr]
  given emptyEnv: VaraEnv = Map.empty

  implicit inline def fromDouble(d: Double): VaraExpr = Const(d)

  implicit inline def fromInt(i: Int): VaraExpr = Const(i.toDouble)

  /* put API */
  case class Put(bindings: (String, VaraExpr)*):
    infix def in(expr: VaraExpr): VaraExpr =
      val env = summon[VaraEnv]
      expr.eval(using env ++ bindings.toMap)

  infix def put(bindings: (String, VaraExpr)*): Put = Put(bindings *)

  infix inline def e(inline s: String): VaraExpr = s

  infix inline def e(inline i: Int): VaraExpr = i

  infix inline def e(inline d: Double): VaraExpr = d
