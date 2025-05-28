package org.vara
import VaraExpr.fromInt

object Calculator:
  @main
  def compute(): Unit =
    val expr: VaraExpr = (("a" +# "b") *# ("f" + "g") #: -2) /# ("b" *# "c" +# "d")
    expr.printAST()
    println(expr)
