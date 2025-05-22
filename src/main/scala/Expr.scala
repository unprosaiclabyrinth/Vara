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
  def simplify: Expr
  
  // Operators
  def +(that: Expr): Expr = ???
  def -(that: Expr): Expr = ???
  def *(that: Expr): Expr = ???
  def /(that: Expr): Expr = ???
  def ^(that: Expr): Expr = ???
  def unary_- : Expr = ???
