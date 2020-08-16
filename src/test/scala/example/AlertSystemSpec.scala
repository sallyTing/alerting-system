package example

import java.time.Instant
import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import AlertSystem._
import io.circe.parser.decode
import io.circe.generic.auto._
import InstantCodec._
import models._

class AlertSystemSpec extends AnyFlatSpec with Matchers {
  def interpreter (rateList: Seq[String], readFileError: Option[Exception] = None, writeFileError: Option[Exception] = None) = new Interpreter[IO] {
    val config = IO(Config())
    def log(str: String) = IO()
    def readRateFile(fileName: String) = readFileError.map(IO.raiseError).getOrElse(IO(rateList))
    def parseToRate(rateString: String) = IO.fromEither(decode[CurrencyConversionRate](rateString))
    def getInput(question: String): IO[String] = IO("outputTest.jsonl")
    def writeAlertFile(fileName: String, alerts: Seq[Alert]) = writeFileError.map(IO.raiseError).getOrElse(IO())
  }

  val xPair = "x"
  val yPair = "y"
  val instantNow = 1594532752.471
  def instantWithMinsAgo(minutes: Int) = instantNow - (minutes * 60.0)
  def toInstance(timeInSecond: Double) = Instant.ofEpochMilli((timeInSecond * 1000).toLong)

  def rateWithTimeValue(value: Double, time: Double = instantNow, pair: String = xPair) =
    s"""{"timestamp":$time,"currencyPair":"$pair","rate":$value}"""
  val rateNow110 = rateWithTimeValue(110.0)

  def ratesWithin5mWithValue(value: Double, pair: String = xPair) =
    (5 to 1 by -1).map(i => rateWithTimeValue(value, instantNow - (i.toDouble), pair)).toList

  def rateBeyond5mWithValue(value: Double, pair: String = xPair) = rateWithTimeValue(value, instantNow - 301.0, pair)
  def rateWithin5mWithValue(value: Double, pair: String = xPair) = rateWithTimeValue(value, instantNow - 300.0, pair)


  "EmptyInput" should "return no alerts" in {
    program.run(interpreter(Seq())).unsafeRunSync() shouldBe(Seq())
  }

  "High value rate" should "not be reported if no history in 5 minutes" in {
    val rateList = Seq(rateNow110)
    program.run(interpreter(rateList)).unsafeRunSync() shouldBe(Seq())
  }

  "High value rate" should "be reported if some history in 5 minutes with low value" in {
    val rateList = Seq(rateBeyond5mWithValue(95.0), rateWithin5mWithValue(Double.MinValue, yPair)) ++ ratesWithin5mWithValue(90.0) ++ Seq(rateNow110, rateWithTimeValue(110.0, instantNow, yPair))

    val reportedAlerts = program.run(interpreter(rateList)).unsafeRunSync()
    reportedAlerts shouldBe(Seq(
      Alert(toInstance(instantNow), xPair, AlertType.spotChange),
      Alert(toInstance(instantNow), yPair, AlertType.spotChange)
    ))
  }

  "Alert System" should "report both spot change and long time change" in {
    val raisingHistory17mAgo = (0 to 10).map(i => rateWithTimeValue(50.0 + i, instantWithMinsAgo(17) + i.toDouble, xPair))
    val raisingHistoryAround2mAgo = (-1 to 1).map(i => rateWithTimeValue(70.0 + i, instantWithMinsAgo(2) + i.toDouble, xPair))
    val raisingHistoryAround1mAgo = (-1 to 1).map(i => rateWithTimeValue(90.0 + i, instantWithMinsAgo(1) + i.toDouble, xPair))
    val rateList = raisingHistory17mAgo ++ raisingHistoryAround2mAgo ++ raisingHistoryAround1mAgo ++ Seq(rateNow110)

    val reportedAlerts =  program.run(interpreter(rateList)).unsafeRunSync()
    reportedAlerts shouldBe(Seq(
      Alert(toInstance(instantWithMinsAgo(17) + 10.0), xPair, AlertType.spotChange),
      Alert(toInstance(instantWithMinsAgo(2)), xPair, AlertType.raising, Some(15*60L)),
      Alert(toInstance(instantWithMinsAgo(1) - 1.0),xPair, AlertType.spotChange),
      Alert(toInstance(instantWithMinsAgo(1)),xPair, AlertType.spotChange),
      Alert(toInstance(instantWithMinsAgo(1)), xPair, AlertType.raising, Some(16*60L)),
      Alert(toInstance(instantWithMinsAgo(1) + 1.0),xPair, AlertType.spotChange),
      Alert(toInstance(instantNow),xPair, AlertType.spotChange),
      Alert(toInstance(instantNow), xPair,AlertType.raising, Some(17*60L))
    ))
  }

  "Alert System" should "throw error if got exception during fetching input file content" in {
    assertThrows[Exception](program.run(interpreter(List(), Some(new Exception("error")))).unsafeRunSync())
  }

  "Alert System" should "throw error if got exception during parsing input file to defined type" in {
    assertThrows[Exception](program.run(interpreter(List("error input"))).unsafeRunSync())
  }

  "Alert System" should "throw error if got exception during writing output file" in {
    assertThrows[Exception](program.run(interpreter(List(), None, Some(new Exception("error")))).unsafeRunSync())
  }
}
