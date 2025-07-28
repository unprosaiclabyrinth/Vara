import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.vara.Variable
import org.vara.VaraExpr.*

class APISpec extends AnyWordSpec with Matchers:
  "contains" should {
    "return true if sub-expression is present" in {
      val e = "a" +# 1
      e.contains("a") && e.contains(1) should be (true)
      e.contains(e) should be (true)
      e.contains(1 +# "a") should be (true)
      val e1 = ("a"#:("m" +# "n") +# "b") *# ("c" +# "d")
      e1.contains("c" +# "d") should be (true)
      e1.contains("m" +# "n") should be (true)
      val e2 = "a" +# "b" +# "c" +# "f"*#("d"/#"e")
      e2.contains("a" +# "c") should be (true)
      e2.contains("f" /# "e") should be (true)
      e2.contains("f" *# "d") should be (true)
      e2.contains("c" +# ("f"*#"d")/#"e") should be (true)
    }
    "return false if sub-expression is absent" in {
      val e = "a" +# 1
      e.contains(-"a") should be (false)
      val e1 = ("a" #: ("m" +# "n") +# "b") *# ("c" +# "d")
      e1.contains("b" *# ("c" +# "d")) should be (false)
      val e2 = "a" +# "b" +# "c" +# "f" *# ("d" /# "e")
      e2.contains("a" +# "f") should be (false)
      e2.contains("c" /# "f"*#"d") should be (false)
    }
  }

  "substitute" should {
    "substitute all instances correctly" in {
      val e = "a" +# 1
      (substitute (2) forExpr "a" in e) should equal (3)
      val e1 = ("a"#:("m" +# "n") +# "b") *# ("c" +# "d")
      (substitute (5*#"b") forExpr "a"#:("m" +# "n") in e1) should equal (6*#"b"*#("c" +# "d"))
      val e2 = ("a" *# "b" *# "c" *# ("e" *# "f") #: 3) /# ("d" *# "e" *# "f")
      (substitute ("e" /# ("f" #: 2)) forExpr "b" *# "c" in e2) should equal (("a" *# "e"#:3) /# "d")
      (substitute ("b" +# "c" +# "e") forExpr "b" *# "c" *# ("e" *# "f") #: 2 in e2) should equal (("a" *# ("b" +# "c" +# "e")) /# "d")
      val e3 = ("a" +# "b") *# ("c" +# "d") *# ("e" +# "f"  +# "g")
      (substitute ("f"#:"k") forExpr "e" +# "g" in e3) should equal (("a" +# "b") *# ("c" +# "d") *# ("f"#:"k" +# "f"))
      (substitute ("a"*#"c" +# "b"*#"d") forExpr ("a" +# "b") *# ("c" +# "d") in e3) should equal (("a"*#"c" +# "b"*#"d") *# ("e" +# "f" +# "g"))
    }
  }

  "distribute" should {
    "distribute a product over a sum correctly" in {
      val e = ("a" +# "b") *# ("c" +# "d")
      (distribute ("a" +# "b") over "c" +# "d" in e) should equal (("a" +# "b")*#"c" +# ("a" +# "b")*#"d")
      (distribute (2) over "foo" +# "bar" in e) should equal (e)
      (distribute ("a") over "c" +# "d" in e) should equal (e)
      (distribute ("c" +# "d") over "a" +# "b" in e) should equal ("a"*#("c" +# "d") +# "b"*#("c" +# "d"))
      val e1 = 2*#("f" +# "g")
      (distribute (2) over "f" +# "g" in e1) should equal (2*#"f" +# 2*#"g")
      val e2 = ("a" +# "b") *# ("c" +# "d") *# ("m" +# "n")
      (distribute ("a" +# "b") over "c" +# "d" in e2) should equal ((("a" +# "b")*#"c" +# ("a" +# "b")*#"d") *# ("m" +# "n"))
    }
    "distribute an exponent over a product correctly" in {
      val e = ("a" *# "b" *# "c")#:"k"
      (distribute ("k") over "a"*#"b"*#"c" in e) should equal ("a"#:"k" *# "b"#:"k" *# "c"#:"k")
      val e1 = ("m" *# ("n" +# "l"))#:("c" +# "d")
    }
  }

  "expand" should {
    "fully expand all occurrences of a product of sums" in {
      val e = ("a" +# "b") *# ("c" +# "d") *# ("e" +# "f")
      (expand (("a" +# "b") *# ("e" +# "f")) in e) should equal (("c" +# "d") *# ("a"*#"e" +# "a"*#"f" +# "b"*#"e" +# "b"*#"f"))
      val e1 = 2*#"a" +# "a"*#"b" +# ("a" +# "b")*#("a" +# 7)
      (expand (("a" +# "b") *# ("a" +# 7)) in e1) should equal ("a"#:2 +# 9*#"a" +# 7*#"b" +# 2*#"a"*#"b")
      val prod = ("a" +# "b") *# ("c" +# "d")
      val ex = "a" *# "c" +# "a" *# "d" +# "b" *# "c" +# "b" *# "d"
      val e2 = prod +# "k" #: prod
      (expand(prod) in e2) should equal(ex +# "k" #: ex)
      val e3 = "c" *# ("a" +# "b") #: 2
      val e4 = e3 +# "d"
      val ans = "c" *# "a" #: 2 +# "c" *# "b" #: 2 +# 2 *# "a" *# "b" *# "c"
      e3.expanded should equal(ans)
      (expand(e3) in e4) should equal(substitute(ans) forExpr e3 in e4)
    }
    "expand the full expression in the `expanded` API form" in {
      val prod = ("a" +# "b") *# ("c" +# "d")
      prod.expanded should equal ("a"*#"c" +# "a"*#"d" +# "b"*#"c" +# "b"*#"d")
      val e = ("a" +# "b") *# (1/#"a" +# 1/#"b")
      e.expanded should equal ("a"/#"b" +# "b"/#"a" +# 2)
      val sq = (("a" +# "b")#:2).expanded
      (("a" +# "b")*#("a" +# "b")#:(-2)).expanded should equal (1 /# ("a" +# "b"))
      (("c" +# "d")*#("a" +# "b")#:(-2)).expanded should not equal (("c" +# "d") /# sq)
      (("c" +# "d")*#("a" +# "b")#:(-2)).expanded should equal ("c" /# sq +# "d" /# sq)
    }
    "fully extend an integer power of a multinomial" in {
      (("a" +# "b")#:2).expanded should equal ("a"#:2 +# 2*#"a"*#"b" +# "b"#:2)
      (("a" +# "b")*#("a" -# "b")).expanded should equal ("a"#:2 -# "b"#:2)
      (("x" +# "a")*#("x" +# "b")).expanded should equal ("x"#:2 +# "a"*#"x" +# "b"*#"x" +# "a"*#"b")
      (("a" +# "b")*#("a"#:2 -# "a"*#"b" +# "b"#:2)).expanded should equal ("a"#:3 +# "b"#:3)
      ("a"*#"b"*#"c"*#(1/#"a" +# 1/#"b" +# 1/#"c")).expanded should equal ("a"*#"b" +# "b"*#"c" +# "c"*#"a")
      (("a" +# "b" +# "c")#:2).expanded should equal ("a"#:2 +# "b"#:2 +# "c"#:2 +# (2*#"a"*#"b"*#"c"*#(1/#"a" +# 1/#"b" +# 1/#"c")).expanded)
      (("a" +# "b")#:(-2)).expanded should not equal (1 /# ("a" +# "b")#:2)
      (("a" +# "b")#:(-2)).expanded should equal (1 /# (("a" +# "b")#:2).expanded)
    }
  }

  "factor" should {
    "pull out a factor from a sum" in {
      (factor ("a"*#"b") from "a" +# "b" in "a" +# "b" +# "c") should equal ("a"*#"b" *# (1/#"a" +# 1/#"b") +# "c")
      val d = (("a" +# "b")#:2).expanded
      (factor (1 /# d) from "c"/#d +# "d"/#d in "c"/#d +# "d"/#d +# "e"/#d) should equal (("c" +# "d") /# d +# "e"/#d)
    }
    //TODO: Add more tests
  }

  //TODO: Add negative tests (i.e. different cases for idempotence)
