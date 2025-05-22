import com.sun.org.apache.xpath.internal.operations.Variable

import scala.language.implicitConversions

/* Expression AST */
trait Expr:
  type Env = Map[String, Expr]
  given emptyEnv: Env = Map.empty

  /** Evaluate an expression to an expression */
  def eval(using env: Env): Expr

  /** Evaluate an expression fully to a double */
  def value(using env: Env): Double = eval match
    case Const(v) => v
    case other => throw new IllegalStateException(s"Unbound variables in: $other")

  /** Simplify/fold constants and basic algebraic rules */
//  def simplify: Expr

  // Operators
  def ++(that: Expr) : Expr = Add(this, that)
  def -(that: Expr): Expr = Sub(this, that)
  def *(that: Expr): Expr = Mul(this, that)
  def /(that: Expr): Expr = Div(this, that)
  def ^(that: Expr): Expr = Pow(this, that)
  def unary_- : Expr = Neg(this)

  extension (e: Expr)
    def put(bindings: (String, Expr)*): Expr =
      given env: Env = bindings.toMap
      e.eval

  private def ast(e: Expr, indent: String): String =
    e match
      case Add(a, b) => s"${indent}Add(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
      case Sub(a, b) => s"${indent}Sub(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
      case Mul(a, b) => s"${indent}Mul(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
      case Div(a, b) => s"${indent}Div(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
      case Pow(a, b) => s"${indent}Pow(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
      case Neg(a) => s"${indent}Neg(\n${ast(a, indent + "  ")}\n$indent)"
      case Const(v) => s"${indent}Const($v)"
      case _ => s"$indent$e"

  override def toString: String = ast(this, "")

object Expr:
  type Env = Map[String, Expr]
  
  implicit def fromDouble(d: Double): Expr = Const(d)

  implicit def fromInt(i: Int): Expr = Const(i.toDouble)
