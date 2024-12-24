package models.recommendations

import play.api.libs.json.{Json, OFormat}

case class RecommendationsResult(data: Seq[Recommendation])

object RecommendationsResult {
  implicit val formats: OFormat[RecommendationsResult] = Json.format[RecommendationsResult]
}
