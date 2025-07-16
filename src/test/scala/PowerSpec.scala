import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.vara.Variable

class PowerSpec extends AnyWordSpec with Matchers:
  "Exponentiation" should {
    "follow power laws" in {
      "a"#:"m" *# "a"#:"n" should equal ("a"#:("m" +# "n"))
      "a"#:"m" /# "a"#:"n" should equal ("a"#:("m" -# "n"))
      ("a"#:"m")#:"n" should equal ("a"#:("m" *# "n"))
    }
    "distribute over a product" in {
      ("a" *# "b")#:"m" should equal ("a"#:"m" *# "b"#:"m")
    }
  }
