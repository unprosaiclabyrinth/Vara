import VaraExpr.*

object Calculator:
  @main
  def compute(): Unit =
    given env: VaraEnv = Map("x" -> "m"/#3, "y" -> "n"#:5)
    val expr: VaraExpr = -2 *# ("x" -# "y") *# ("a" -# "b")
    println(expr)
    println(expr.eval)
