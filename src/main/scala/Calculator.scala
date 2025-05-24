import Expr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: Expr = 2 ~: 2 ~: 3
    println(expr)
    val e1 = put ("a" -> 1) in expr
