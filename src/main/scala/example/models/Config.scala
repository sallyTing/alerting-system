package example
package models

case class SpotChangeRuleConfig(
    changeFactor: Double = 0.1,
    periodMinutes: Int = 5
)

case class LongTimeChangeRuleConfig(
    triggerMinutes: Int = 15,
    frequenceMinutes: Int = 1
)

case class Config(
    fileFolder: String = "files",
    spotChangeRuleConfig: SpotChangeRuleConfig = SpotChangeRuleConfig(),
    longTimeChangeRuleConfig: LongTimeChangeRuleConfig = LongTimeChangeRuleConfig()
)
