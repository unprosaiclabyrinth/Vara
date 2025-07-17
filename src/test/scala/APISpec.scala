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

  "replace" should {
    "replace all instances correctly" in {
      val e = "a" +# 1
      (replace ("a") withExpr 2 in e) should equal (3)
      val e1 = ("a"#:("m" +# "n") +# "b") *# ("c" +# "d")
      (replace ("a"#:("m" +# "n")) withExpr 5*#"b" in e1) should equal (6*#"b"*#("c" +# "d"))
      val e2 = ("a" *# "b" *# "c" *# ("e" *# "f") #: 3) /# ("d" *# "e" *# "f")
      (replace("b" *# "c") withExpr "e" /# ("f" #: 2) in e2) should equal (("a" *# "e"#:3) /# "d")
      (replace ("b" *# "c" *# ("e" *# "f") #: 2) withExpr "b" +# "c" +# "e" in e2) should equal (("a" *# ("b" +# "c" +# "e")) /# "d")
      val e3 = ("a" +# "b") *# ("c" +# "d") *# ("e" +# "f"  +# "g")
      (replace ("e" +# "g") withExpr "f"#:"k" in e3) should equal (("a" +# "b") *# ("c" +# "d") *# ("f"#:"k" +# "f"))
      (replace (("a" +# "b") *# ("c" +# "d")) withExpr "a"*#"c" +# "b"*#"d" in e3) should equal (("a"*#"c" +# "b"*#"d") *# ("e" +# "f" +# "g"))
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
    }
  }
