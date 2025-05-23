import scala.language.{implicitConversions, postfixOps}

/* Expression AST */
trait Expr:
  type Env = Map[String, Expr]
  given emptyEnv: Env = Map.empty

  /** Evaluate an expression partially or fully from the given environment */
  def eval(using env: Env): Expr

  /** Evaluate an expression fully to a value */
  def value(using env: Env): Either[Double, Exception] = eval match
    case Const(v) => Left(v)
    case other => Right(new IllegalStateException(s"Unbound variables in: $other"))

  // Operators (according to correct precedence and assoc)
  def |:(that: Expr): Expr = Pow(this, that)

  def *|(that: Expr): Expr = Mul2(this, that)

  def /~(that: Expr): Expr = Div(this, that)

  def +|(that: Expr) : Expr = Add2(this, that)

  def -|(that: Expr): Expr = Sub(this, that)

  def unary_- : Expr = Neg(this)

  private def ast(e: Expr, indent: String): String = e match
    case Add(h, t*) =>
      s"${indent}Add(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Mul(h, t*) =>
      s"${indent}Mul(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Pow(a, b) => s"${indent}Pow(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
    case Const(v) => s"${indent}Const($v)"
    case _ => s"$indent$e"

  override def toString: String = ast(this, "")

object Expr:
  type Env = Map[String, Expr]
  given emptyEnv: Env = Map.empty

  implicit def fromDouble(d: Double): Expr = Const(d)

  implicit def fromInt(i: Int): Expr = Const(i.toDouble)
