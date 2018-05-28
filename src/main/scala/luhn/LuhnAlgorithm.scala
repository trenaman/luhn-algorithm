
package luhn

object LuhnAlgorithm {
  val InputIsEmptyErrorMessage = "Input cannot be an empty sequence."
  val InputContainsNonDigitsErrorMessage = "All digits must between 0 and 9"

  // For a given sequence of digits, return a check digit as per Luhn algorithm.
  //
  def generateChecksum(s: String): Int = {
    generateChecksum(s.map(digitToInt(_)))
  }

  def generateChecksum(seq: Seq[Int]): Int = {
    validateInputSequence(seq)

    (luhnTransform(seq.reverse).sum * 9) % 10
  }

  // Returns true if the sequence is valid as per Luhn algorithm.
  //
  def validate(s: String): Boolean = {
    validate(s.map(digitToInt(_)))
  }

  def validate(seq: Seq[Int]): Boolean = {
    validateInputSequence(seq)

    val reversedSeq = seq.reverse

    val checksum = reversedSeq.head
    val digits = reversedSeq.tail

    (checksum + luhnTransform(digits).sum) % 10 == 0
  }

  // Double every odd element in the sequence and sum the digits of the doubling, as per the Luhn algorithm
  //
  private def luhnTransform(seq: Seq[Int]): Seq[Int] = {
    lazy val oddElements: Stream[Boolean] = true #:: oddElements.map(b => !b)

    seq.zip(oddElements).map { case (d, isOddElement) =>
      if (isOddElement)
        sumDigits(d * 2)
      else
        d
    }
  }

  private def sumDigits(d: Int): Int = {
    if (d < 10)
      d
    else
      1 + (d % 10)
  }

  private def digitToInt(c: Char): Int = c.toInt - '0'.toInt

  private def validateInputSequence(seq: Seq[Int]): Unit = {
    require(seq.nonEmpty, InputIsEmptyErrorMessage)
    require(
      seq.forall { n =>
        n >= 0 && n <= 9
      },
      InputContainsNonDigitsErrorMessage
    )
  }

}
