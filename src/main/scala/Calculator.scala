import Expr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: Expr = 2 ** ("a" ++ "b")
    println(expr)
