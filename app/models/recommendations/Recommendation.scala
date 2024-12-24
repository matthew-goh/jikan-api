package models.recommendations

import play.api.libs.json.{Json, OFormat}

case class Recommendation(entry: RecommendationEntry, votes: Int)

object Recommendation {
  implicit val formats: OFormat[Recommendation] = Json.format[Recommendation]
}
