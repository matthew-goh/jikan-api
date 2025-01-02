package models.statistics

import play.api.libs.json.{Json, OFormat}

case class Score(score: Int, votes: Int, percentage: Double)

object Score {
  implicit val formats: OFormat[Score] = Json.format[Score]
}
