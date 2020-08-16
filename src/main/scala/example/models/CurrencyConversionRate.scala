package example
package models

import java.time.Instant
import io.circe.Encoder


case class CurrencyConversionRate(
                                   timestamp: Instant,
                                   currencyPair: String,
                                   rate: Double
                                 ) {
  def toLongTimeChangeAlert(startRate: CurrencyConversionRate): Alert = Alert(
    this.timestamp,
    this.currencyPair,
    if (this.rate - startRate.rate > 0.0) AlertType.raising else AlertType.falling,
    Some(this.timestamp.getEpochSecond - startRate.timestamp.getEpochSecond)
  )

  def toSpotChangeAlert(): Alert = Alert(
    this.timestamp,
    this.currencyPair,
    AlertType.spotChange
  )
}

object AlertType extends Enumeration {
  val spotChange, falling, raising = Value

  implicit val encoder: Encoder[Value] = Encoder.encodeString.contramap[Value](_.toString)
}

case class Alert(
    timestamp: Instant,
    currencyPair: String,
    alert: AlertType.Value,
    seconds: Option[Long] = None
)

case class SpotChangeRule(
    fiveMinutesHistory: Seq[CurrencyConversionRate],
)

case class LongTimeChangeRule(
    diff: Double,
    triggerTime: Option[Instant],
    sameTrendingHistory: Seq[CurrencyConversionRate]
)

case class AlertResult(
    spotChangeRules: Map[String, SpotChangeRule],
    longTimeChangeRules: Map[String, LongTimeChangeRule],
    alerts: Seq[Alert]
)
