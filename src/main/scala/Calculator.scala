import VaraExpr.*

object Calculator:
  @main
  def compute(): Unit =
    val expr: VaraExpr = "a"*~"b" +~ 2*~"b"*~"a"
    println(expr)
