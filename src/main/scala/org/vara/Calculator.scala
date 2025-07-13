package org.vara
import VaraExpr.*

object Calculator:
  private def f(i: VaraExpr): VaraExpr =
    // complex computation
    (1 to 5).foldLeft(e(0))(
      (acc, _) => acc +# i
    )
    /*
     sum = 0;
     for (j = 1; j <= 5; j++) {
         sum += i;
     }
     return sum;
     */

  @main
  def compute(): Unit =
    val e = f("x")
    val f5 = put ("x" -> 5) in e
    println(f5)
