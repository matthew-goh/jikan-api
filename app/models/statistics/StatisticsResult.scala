package models.statistics

import play.api.libs.json.{Json, OFormat}

case class StatisticsResult(data: AnimeStats)

object StatisticsResult {
  implicit val formats: OFormat[StatisticsResult] = Json.format[StatisticsResult]
}
