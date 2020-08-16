package example

import java.time.Instant
import models._

object AlertRuleService {
  def passIndividualRate(config: Config): (AlertResult, CurrencyConversionRate) => AlertResult =
    (oldResult: AlertResult, rate: CurrencyConversionRate) => {
      val pair = rate.currencyPair
      val (newSpotChangeRule, alert1) = runSpotChangeRule(rate, oldResult.spotChangeRules.get(rate.currencyPair), config.spotChangeRuleConfig)
      val (newLongTimeChangeRule, alert2) = runLongTimeChangeRule(rate, oldResult.longTimeChangeRules.get(rate.currencyPair), config.longTimeChangeRuleConfig)
      AlertResult(
        oldResult.spotChangeRules + (pair -> newSpotChangeRule),
        oldResult.longTimeChangeRules + (pair -> newLongTimeChangeRule),
        oldResult.alerts ++ alert1.toSeq ++ alert2.toSeq
      )
    }

  private[example] def runSpotChangeRule(rate: CurrencyConversionRate, spotChangeRule: Option[SpotChangeRule], config: SpotChangeRuleConfig): (SpotChangeRule, Option[Alert]) = {
    val fiveMinutesHistory = getFiveMinutesHistoryFrom(spotChangeRule)
    fiveMinutesHistory.dropWhile(r => r.timestamp.isBefore(rate.timestamp.minusSeconds(config.periodMinutes * 60L))) match {
      case fiveMinutes: Seq[CurrencyConversionRate] if fiveMinutes.nonEmpty => {
        val average = fiveMinutes.map(_.rate).sum / fiveMinutes.length.toDouble
        val alert = if ((rate.rate - average).abs / average.abs > config.changeFactor)
          Some(rate.toSpotChangeAlert())
        else None
        (SpotChangeRule(fiveMinutes :+ rate), alert)
      }
      case _ => (SpotChangeRule(Seq(rate)), None)
    }
  }

  private def getFiveMinutesHistoryFrom(spotChangeRule: Option[SpotChangeRule]) = {
    spotChangeRule.map(_.fiveMinutesHistory).getOrElse(Seq())
  }

  private[example] def runLongTimeChangeRule(rate: CurrencyConversionRate, longTimeChangeRule: Option[LongTimeChangeRule], config: LongTimeChangeRuleConfig): (LongTimeChangeRule, Option[Alert]) = {
    longTimeChangeRule match {
      case Some(LongTimeChangeRule(diff, triggerTime, history)) if history.nonEmpty && diff != 0.0 =>
        val newDiff = rate.rate - history.last.rate
        (isSameSign(newDiff, diff), triggerTime) match {
          case (true, Some(trigger)) => {
            if (hasPassedMinutes(rate.timestamp, trigger, config.frequenceMinutes))
              (LongTimeChangeRule(newDiff, Some(rate.timestamp), history :+ rate), Some(rate.toLongTimeChangeAlert(history.head)))
            else
              (LongTimeChangeRule(newDiff, Some(trigger), history :+ rate), None)
          }
          case (true, _) => {
            if (hasPassedMinutes(rate.timestamp, history.head.timestamp, config.triggerMinutes))
              (LongTimeChangeRule(newDiff, Some(rate.timestamp), history :+ rate), Some(rate.toLongTimeChangeAlert(history.head)))
            else
              (LongTimeChangeRule(newDiff, None, history :+ rate), None)
          }
          case (false, _) => (LongTimeChangeRule(newDiff, None, Seq(history.last, rate)), None)

        }
      case Some(LongTimeChangeRule(_, _, history)) if history.nonEmpty =>
        (LongTimeChangeRule(
          rate.rate - history.last.rate,
          None,
          Seq(history.last, rate)),
          None)
      case _ => (LongTimeChangeRule(0.0, None, Seq(rate)), None)
    }
  }

  private def isSameSign(a: Double, b: Double) = if (a * b > 0.0) true else false

  private def hasPassedMinutes(recentTime: Instant, historyTime: Instant, minutes: Int) = recentTime.getEpochSecond - historyTime.getEpochSecond >= minutes * 60L

}
