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
  def +(that: Expr): Expr = Add(this, that)
  def -(that: Expr): Expr = Sub(this, that)
  def *(that: Expr): Expr = Mul(this, that)
  def /(that: Expr): Expr = Div(this, that)
  def ^(that: Expr): Expr = Pow(this, that)
  def unary_- : Expr = Neg(this)

  implicit def fromDouble(d: Double): Expr = Const(d)
  implicit def fromInt(i: Int): Expr = Const(i.toDouble)

  extension (e: Expr)
    def put(bindings: (String, Expr)*): Expr =
      given env: Env = bindings.toMap
      e.eval
