import Expr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr = 2 ++ "a" ++ "b" ++ ("a" ++ 3)
    println(expr)
