package example

import java.time.Instant
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AlertRuleServiceSpec extends AnyFlatSpec with Matchers {

  val spotChangeRuleConfig = SpotChangeRuleConfig(
    changeFactor = 0.1,
    periodMinutes = 5
  )
  val longTimeChangeRuleConfig = LongTimeChangeRuleConfig(
    triggerMinutes = 15,
    frequenceMinutes = 1
  )

  val xPair = "x"
  val instantNow = Instant.now()
  val instant15MinAgo = instantNow.minusSeconds(15 * 60L)
  def instanceWithSecondsAgo(seconds: Long) = instantNow.minusSeconds(seconds)

  def rateNowWithValue(value: Double, pair: String = xPair) = CurrencyConversionRate(instantNow, pair, value)
  val rateNow110 = rateNowWithValue(110.0)
  val rateNow90 = rateNowWithValue(90.0)

  def ratesWithin5mWithValue(value: Double, pair: String = xPair) =
    (5 to 1 by -1).map(i => CurrencyConversionRate(instantNow.minusSeconds(i.toLong), pair, value ))

  def rateBeyond5mWithValue(value: Double, pair: String = xPair) = CurrencyConversionRate(instantNow.minusSeconds(301L), pair, value)
  def rateWithin5mWithValue(value: Double, pair: String = xPair) = CurrencyConversionRate(instantNow.minusSeconds(300L), pair, value)

  val fallingHistoryWithin15m = (1 to 10).map(i => CurrencyConversionRate(instant15MinAgo.plusSeconds(i.toLong), xPair, 110.0 - i))
  val fallingHistoryFor15m = (0 to 10).map(i => CurrencyConversionRate(instant15MinAgo.plusSeconds(i.toLong), xPair, 110.0 - i))
  val raisingHistoryFor15m = (0 to 10).map(i => CurrencyConversionRate(instant15MinAgo.plusSeconds(i.toLong), xPair, 70.0 + i))
  val sameHistoryFor15m = (0 to 10).map(i => CurrencyConversionRate(instant15MinAgo.plusSeconds(i.toLong), xPair, 90.0))


  "Spot Change Rule" should "return no alerts with no rule history" in {
    AlertRuleService.runSpotChangeRule(rateNowWithValue(Double.MaxValue), None, spotChangeRuleConfig)._2 shouldBe(None)
  }

  "Spot Change Rule" should "return no alerts if no history in 5 minutes" in {
    AlertRuleService.runSpotChangeRule(rateNow110, Some(SpotChangeRule(Seq(rateBeyond5mWithValue(Double.MinValue)))), spotChangeRuleConfig)._2 shouldBe(None)
  }

  "Spot Change Rule" should "return alert if some history in 5 minutes with low value and regardless any rate more than 5 minutes ago" in {
    val history1 = Seq(rateWithin5mWithValue(90.0))
    val history2 = Seq(rateBeyond5mWithValue(130.0),rateWithin5mWithValue(90.0))
    AlertRuleService.runSpotChangeRule(rateNow110, Some(SpotChangeRule(history1)), spotChangeRuleConfig)._2 shouldNot be(None)
    AlertRuleService.runSpotChangeRule(rateNow110, Some(SpotChangeRule(history2)), spotChangeRuleConfig)._2 shouldNot be(None)
  }

  "Spot Change Rule" should "return no alert if change within 10%" in {
    val history = ratesWithin5mWithValue(100.0)
    AlertRuleService.runSpotChangeRule(rateNow110, Some(SpotChangeRule(history)), spotChangeRuleConfig)._2 shouldBe(None)
    AlertRuleService.runSpotChangeRule(rateNow90, Some(SpotChangeRule(history)), spotChangeRuleConfig)._2 shouldBe(None)
  }

  "Spot Change Rule" should "return alert if change greater than 10%" in {
    val historyWithAverage99 = ratesWithin5mWithValue(99.0)
    val historyWithAverage101 = ratesWithin5mWithValue(101.0)
    AlertRuleService.runSpotChangeRule(rateNow110, Some(SpotChangeRule(historyWithAverage99)), spotChangeRuleConfig)._2 shouldNot be(None)
    AlertRuleService.runSpotChangeRule(rateNow90, Some(SpotChangeRule(historyWithAverage101)), spotChangeRuleConfig)._2 shouldNot be(None)
  }

  "Long time change rule" should "return no alert if no rule history" in {
    AlertRuleService.runLongTimeChangeRule(rateNow90, None, longTimeChangeRuleConfig)._2 shouldBe(None)
  }

  "Long time change rule" should "return no alert if keep same change within 15 minutes" in {
    AlertRuleService.runLongTimeChangeRule(rateNow90, Some(LongTimeChangeRule(-1.0, None, fallingHistoryWithin15m)), longTimeChangeRuleConfig)._2 shouldBe(None)
  }


  "Long time change rule" should "return no alert if keep same change for 15 minutes" in {
    val (longTimeChangeRule1, genFallingAlert) = AlertRuleService.runLongTimeChangeRule(rateNow90, Some(LongTimeChangeRule(-1.0, None, fallingHistoryFor15m)), longTimeChangeRuleConfig)
    genFallingAlert.map(_.alert).get shouldBe(AlertType.falling)
    genFallingAlert.flatMap(_.seconds).get shouldBe(15*60L)
    longTimeChangeRule1.sameTrendingHistory.length shouldBe(12)
    longTimeChangeRule1.triggerTime.get shouldBe(instantNow)

    val (longTimeChangeRule2, genRaisingAlert) = AlertRuleService.runLongTimeChangeRule(rateNow90, Some(LongTimeChangeRule(1.0, None, raisingHistoryFor15m)), longTimeChangeRuleConfig)
    genRaisingAlert.map(_.alert).get shouldBe(AlertType.raising)
    genRaisingAlert.flatMap(_.seconds).get shouldBe(15*60L)
    longTimeChangeRule2.sameTrendingHistory.length shouldBe(12)
    longTimeChangeRule2.triggerTime.get shouldBe(instantNow)
  }

  "Long time change rule" should "stop same trending history if get different trend" in {
    val (longTimeChangeRule, alert) = AlertRuleService.runLongTimeChangeRule(rateNow110, Some(LongTimeChangeRule(-1.0, None, fallingHistoryFor15m)), longTimeChangeRuleConfig)
    alert shouldBe(None)
    longTimeChangeRule.sameTrendingHistory shouldBe(Seq(fallingHistoryFor15m.last, rateNow110))
    longTimeChangeRule.triggerTime shouldBe(None)
  }

  "Long time change rule" should "return no alert with no change from last one" in {
    val currentRate = rateNowWithValue(fallingHistoryWithin15m.last.rate)
    val (longTimeChangeRule, alert) = AlertRuleService.runLongTimeChangeRule(currentRate, Some(LongTimeChangeRule(-1.0, None, fallingHistoryWithin15m)), longTimeChangeRuleConfig)
    longTimeChangeRule.sameTrendingHistory shouldBe(Seq(fallingHistoryFor15m.last, currentRate))
    alert shouldBe(None)
  }

  "Long time change rule" should "return no alert if less than 1 min since last trigger" in {
    val lessThan1minAgo = instanceWithSecondsAgo(59L)
    val history = fallingHistoryFor15m :+ CurrencyConversionRate(lessThan1minAgo, xPair, 91.0)
    val (longTimeChangeRule, alert) = AlertRuleService.runLongTimeChangeRule(rateNow90, Some(LongTimeChangeRule(-1.0, Some(lessThan1minAgo), history)), longTimeChangeRuleConfig)
    alert shouldBe(None)
    longTimeChangeRule.triggerTime.get shouldBe(lessThan1minAgo)
    longTimeChangeRule.sameTrendingHistory.length shouldBe(13)
  }

  "Long time change rule" should "return alert if last trigger is equal or greater than 1 minute" in {
    val lessThan1minAgo = instanceWithSecondsAgo(59L)
    val oneMinAgo = instanceWithSecondsAgo(60L)
    val history = fallingHistoryFor15m :+ CurrencyConversionRate(oneMinAgo, xPair, 99.0) :+ CurrencyConversionRate(lessThan1minAgo, xPair, 91.0)
    val (longTimeChangeRule, genFallingAlert) = AlertRuleService.runLongTimeChangeRule(rateNow90, Some(LongTimeChangeRule(-1.0, Some(oneMinAgo), history)), longTimeChangeRuleConfig)
    genFallingAlert.map(_.alert).get shouldBe(AlertType.falling)
    genFallingAlert.flatMap(_.seconds).get shouldBe(15*60L)
    longTimeChangeRule.triggerTime.get shouldBe(instantNow)
    longTimeChangeRule.sameTrendingHistory.length shouldBe(14)
  }

}
