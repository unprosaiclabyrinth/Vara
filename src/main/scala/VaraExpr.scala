import scala.language.{implicitConversions, postfixOps}
import scala.util.{Failure, Success, Try}

/* Expression AST */
trait VaraExpr:
  type Env = Map[String, VaraExpr]
  given emptyEnv: Env = Map.empty

  /** Evaluate an expression partially or fully from the given environment */
  def eval(using env: Env): VaraExpr

  /** Evaluate an expression fully to a value */
  def value(using env: Env): Try[Double] = eval match
    case Const(v) => Success(v)
    case other => Failure(new IllegalStateException(s"Unbound variables in: $other"))

  // Operators (according to correct precedence and assoc)
  def ~:(that: VaraExpr): VaraExpr = Pow(that, this) // right associativity

  def *~(that: VaraExpr): VaraExpr = Mul2(this, that)

  def /~(that: VaraExpr): VaraExpr = Div(this, that)

  def +~(that: VaraExpr) : VaraExpr = Add2(this, that)

  def -~(that: VaraExpr): VaraExpr = Sub(this, that)

  def unary_- : VaraExpr = Neg(this)

  private def ast(e: VaraExpr, indent: String): String = e match
    case Add(h, t*) =>
      s"${indent}Add(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Mul(h, t*) =>
      s"${indent}Mul(\n${ast(h, indent + "  ")}${t.foldLeft("")((acc, e) => acc + s",\n${ast(e, indent + "  ")}")}\n$indent)"
    case Pow(a, b) => s"${indent}Pow(\n${ast(a, indent + "  ")},\n${ast(b, indent + "  ")}\n$indent)"
    case Const(v) => s"${indent}Const($v)"
    case _ => s"$indent$e"

  override def toString: String = ast(this, "")

object VaraExpr:
  type Env = Map[String, VaraExpr]
  given emptyEnv: Env = Map.empty

  implicit def fromDouble(d: Double): VaraExpr = Const(d)

  implicit def fromInt(i: Int): VaraExpr = Const(i.toDouble)

  /* put API */
  case class Put(bindings: (String, VaraExpr)*):
    infix def in(expr: VaraExpr): VaraExpr =
      val env = summon[Env]
      expr.eval(using env ++ bindings.toMap)

  infix def put(bindings: (String, VaraExpr)*): Put = Put(bindings *)
