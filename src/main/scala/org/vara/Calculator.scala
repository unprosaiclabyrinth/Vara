package org.vara

object Calculator:
  @main
  def compute(): Unit =
    val expr: VaraExpr = ("a" +# "b") /# ("b" *# "c" +# "d")
    expr.printAST()
    println(expr)
