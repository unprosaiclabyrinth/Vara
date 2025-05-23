import Expr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: Expr = "a"++"a"--2
    println(expr)
