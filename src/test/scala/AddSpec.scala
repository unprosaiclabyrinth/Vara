import VaraExpr.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class AddSpec extends AnyWordSpec with Matchers:
  "Add" should {
    "add two constants correctly" in {
      2 +~ 2 should equal (4)
      411 +~ 511 should equal (922)
      3.14 +~ 2.71 should equal (5.85)
      -5 +~ 5 should equal (0)
      -7.89 +~ 1.23 should equal (-6.66)
      0 + 0 should be (0)
    }
    "add two variables correctly" in {
      "a" +~ "b" should equal ("a" +~ "b")
      "a" +~ "b" should not equal ("A" +~ "b")
      "a" +~ "b" should not equal "ab"
      "a" +~ "b" +~ "c" should equal ("a" +~ "b" +~ "c")
      "a" +~ "b" +~ "c" should not equal "abc"
    }
    "be commutative" in {
      2 +~ 3 should equal (3 +~ 2)
      "b" +~ "a" should equal ("b" +~ "a")
    }
    "be associative" in {
      "a" +~ ("b" +~ "c") should equal (("a" +~ "b") +~ "c")
    }
    "add constants and variables correctly" in {
      2 +~ "a" should equal (2 +~ "a")
      "a" +~ 2 should equal (2 +~ "a")
      2 +~ "a" should not equal "2a"
    }
    "fold constants" when {
      "mixed with variables" in {
        2 +~ "a" +~ 3 should equal(5 +~ "a")
        1 +~ "a" +~ 2 +~ "b" +~ 1 +~ "c" should equal(4 +~ "a" +~ "b" +~ "c")
        (1 to 100).foldLeft(e(0))((acc, _) => acc +~ 1 +~ "a") should equal(100 +~ 100 *~ "a")
      }
    }
    "add like terms together" in {
      "a" +~ "a" should equal (2*~"a")
      "a" +~ "b" +~ "a" should equal (2*~"a" +~ "b")
      5*~"a" +~ 7*~"a" should equal (12*~"a")
    }
    "not change terms involving exponentiation" in {
      "a"~:"A" +~ "b"~:"B" should equal ("a"~:"A" +~ "b"~:"B")
      "a" +~ "a"~:2 +~ "a"~:3 +~ "a"~:"b" should equal ("a" +~ "a"~:2 +~ "a"~:3 +~ "a"~:"b")
    }
  }

  "Addition with 0" should {
    "return the other addends and ignore the zeroes" in {
      "a" +~ 0 should equal ("a")
      0 +~ "a" +~ 0 should equal ("a")
      "a" +~ 0 +~ 0 +~ "b" +~ 0 +~ "c" should equal ("a" +~ "b" +~ "c")
      (1 to 1000).foldLeft(e(0))((acc, _) => acc +~ 0 +~ "a") should equal ("a"*~1000)
    }
  }
