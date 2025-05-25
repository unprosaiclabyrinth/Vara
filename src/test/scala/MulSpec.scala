import VaraExpr.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class MulSpec extends AnyWordSpec with Matchers:
  "Times" should {
    "multiply two constants correctly" in {
      2 *# 2 should equal(4)
      411 *# 511 should equal(411 * 511)
      3.14 *# 2.71 should equal(3.14 * 2.71)
      -5 *# 5 should equal(-25)
      -7.89 *# -1.23 should equal(7.89 * 1.23)
      0 *# 0 should be(0)
    }
    "multiply two variables correctly" in {
      "a" *# "b" should equal("a"*#"b")
      "a" *# "b" should not equal ("A"*#"b")
      "a" *# "b" should not equal "ab"
      "a" *# "b" *# "c" should equal ("a"*#"b"*#"c")
      "a" *# "b" *# "c" should not equal "abc"
    }
    "be commutative" in {
      2 *# 3 should equal(3 *# 2)
      "b" *# "a" should equal("b" *# "a")
    }
    "be associative" in {
      "a" *# ("b" *# "c") should equal(("a" *# "b") *# "c")
    }
    "multiply constants and variables correctly" in {
      2 *# "a" should equal (2*#"a")
      "a" *# 2 should equal (2*#"a")
      2*#"a" should not equal "2a"
    }
    "fold constants" when {
      "mixed with variables" in {
        2 *# "a" *# 3 should equal (6*#"a")
        4 *# "a" *# 2 *# "b" *# 4 *# "c" should equal (32*#"a"*#"b"*#"c")
      }
    }
    "exponentiate like terms" in {
      "a" *# "a" should equal("a"#:2)
      "a" *# "b" *# "a" should equal("b" *# "a"#:2)
      "a"#:5 *# "a"#:7 should equal("a"#:12)
      "a"#:"b" *# "a"#:"c" should equal ("a"#:("b" +# "c"))
    }
    "not change terms involving exponentiation and different bases" in {
      "a"#:"A" *# "b"#:"B" should equal("a"#:"A" *# "b"#:"B")
    }
  }

  "Multiplication with 1" should {
    "return the other terms and ignore the ones" in {
      "a" *# 1 should equal ("a")
      1 *# "a" *# 1 should equal ("a")
      "a" *# 1 *# 1 *# "b" *# 1 *# "c" should equal ("a"*#"b"*#"c")
      (1 to 1000).foldLeft(e(1))((acc, _) => acc*#1*#"a") should equal("a"#:1000)
    }
  }

  "Multiplication with 0" should {
    "give 0 irrespective of the other terms" in {
      "a" *# 0 should equal (0)
      0 *# "x" *# "y" *# "z" should equal (0)
      0 *# "a"#:("b"#:"c" *# "d"#:"x") should equal (0)
    }
  }

  "Multiplication of a sum with a sum" should {
    "give the sum of the products by distribution" in {
      2 *# ("x" +# "y") should equal (2*#"x" +# 2*#"y")
      (1 +# 2 +# 3) *# ("a" +# "b" +# "c") should equal (6*#"a" +# 6*#"b" +# 6*#"c")
      2 *# ("x" +# "y") +# ("a" +# "b") should equal (2 *# "x" +# 2 *# "y" +# "a" +# "b")
      2 *# ("x" +# "y") *# ("a" +# "b") should equal (2*#"x"*#"a" +# 2*#"y"*#"a" +# 2*#"x"*#"b" +# 2*#"y"*#"b")
    }
  }
