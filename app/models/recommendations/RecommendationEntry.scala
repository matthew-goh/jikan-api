package models.recommendations

import models.Images
import play.api.libs.json.{Json, OFormat}

case class RecommendationEntry(mal_id: Int, title: String, images: Images)

object RecommendationEntry {
  implicit val formats: OFormat[RecommendationEntry] = Json.format[RecommendationEntry]
}
