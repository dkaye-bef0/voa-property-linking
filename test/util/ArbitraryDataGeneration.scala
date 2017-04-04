package util

import org.scalacheck.Gen

trait ArbitraryDataGeneration {

  implicit def generate[T](t: Gen[T]): T = t.sample.get

  def shortString: Gen[String] = Gen.listOfN(20, Gen.alphaNumChar).map(_.mkString)


}
