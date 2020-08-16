package example

import models._

trait Interpreter[F[_]] {
  val config: F[Config]
  def log(str: String): F[Unit]
  def readRateFile(fileName: String): F[Seq[String]]
  def parseToRate(rateString: String): F[CurrencyConversionRate]
  def getInput(question: String): F[String]
  def writeAlertFile(fileName: String, alerts: Seq[Alert]): F[Unit]
}