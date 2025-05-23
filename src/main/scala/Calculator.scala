import Expr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: Expr = 2**"a" -- (2**"a" -- 1)
    println(expr)
