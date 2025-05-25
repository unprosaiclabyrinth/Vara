import VaraExpr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: VaraExpr = ("x" +~ "y") *~ ("a" +~ "b")
    println(expr)
