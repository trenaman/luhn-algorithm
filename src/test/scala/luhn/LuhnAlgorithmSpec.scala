
package luhn

import org.scalatest.{FlatSpec, Matchers}
import LuhnAlgorithm._

class LuhnAlgorithmSpec extends FlatSpec with Matchers {

  "generateChecksum" should "return 2 for card 123456789012345" in {
    assertResult(2) {
      generateChecksum("123456789012345")
    }
  }

  it should "return 3 for 7992739871" in {
    assertResult(3) {
      generateChecksum("7992739871")
    }
  }

  it should " throw IllegalArgumentException if input sequence is empty" in {
    assertThrows[IllegalArgumentException] {
      generateChecksum("")
    }
  }

  it should "throw IllegalArgumentException if input sequence has non-numeric digits" in {
    assertThrows[IllegalArgumentException] {
      validate("aB09ds90-^&*")
    }
  }

  "validate" should "return true for card 1234567890123452" in {
    assertResult(true) {
      validate("1234567890123452")
    }
  }

  it should "return true for 79927398713" in {
    assertResult(true) {
      validate("79927398713")
    }
  }

  it should "return false for all sequences starting with 7992739871 and not ending in 3" in {
    val invalidNums = Seq(
      "79927398710",
      "79927398711",
      "79927398712",
      "79927398714",
      "79927398715",
      "79927398716",
      "79927398717",
      "79927398718",
      "79927398719"
    )

    val validationResponses = invalidNums.map(validate(_))

    assertResult(false) {
      validationResponses.foldLeft(false)(_ || _)
    }
  }

  it should "throw IllegalArgumentException if input sequence is empty" in {
    assertThrows[IllegalArgumentException] {
      validate("")
    }
  }

  it should "throw IllegalArgumentException if input sequence has non-numeric digits" in {
    assertThrows[IllegalArgumentException] {
      validate("aB09ds90-^&*")
    }
  }
}
