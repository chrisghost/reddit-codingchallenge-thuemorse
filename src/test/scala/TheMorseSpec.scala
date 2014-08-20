package test

import thue.morse._
import org.specs2.mutable._

class TheMorseSpec extends Specification {
  def resultAsString(result: List[Boolean]) = result.map { case true => 1 case _ => 0 }.mkString("")
  "ThueMorse" should {
    "compute the thue-morse value correctly" in {
      resultAsString(ThueMorse.iterCompute(6)) mustEqual "0110100110010110100101100110100110010110011010010110100110010110"
      ThueMorse.bitCompute(6) mustEqual "0110100110010110100101100110100110010110011010010110100110010110"
    }
  }
}
