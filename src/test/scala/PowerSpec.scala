import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.vara.VaraExpr.fromInt
import org.vara.Variable

class PowerSpec extends AnyWordSpec with Matchers:
  "Exponentiation" should {
    "follow laws of indices" in {
      "a"#:"m" *# "a"#:"n" should equal ("a"#:("m" +# "n"))
      "a"#:"m" /# "a"#:"n" should equal ("a"#:("m" -# "n"))
      "a"#:"k" /# "a" should equal ("a"#:("k" -# 1))
      ("a"#:"m")#:"n" should equal ("a"#:("m" *# "n"))
    }
    "distribute constant powers over a product" in {
      ("a" *# "b")#:10 should equal ("a"#:10 *# "b"#:10)
      "a"#:(2025 +# "k") *# ("a" *# "b")#:(-2025) should equal ("a"#:"k" /# "b"#:2025)
    }
  }
