package models.statistics

import be.venneborg.refined.play.RefinedJsonFormats._
import models.RefinedTypes._
import play.api.libs.json.{Json, OFormat}

case class Score(score: ScoreInt, votes: NaturalNum, percentage: Percentage)

object Score {
  implicit val formats: OFormat[Score] = Json.format[Score]
}
