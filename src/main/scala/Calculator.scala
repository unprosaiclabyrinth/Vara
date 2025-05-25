import VaraExpr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: VaraExpr = -2 *~ ("x" -~ "y") *~ ("a" -~ "b")
    println(expr)
