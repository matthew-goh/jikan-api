package models.recommendations

import be.venneborg.refined.play.RefinedJsonFormats._
import models.Images
import models.RefinedTypes.UrlString
import play.api.libs.json.{Json, OFormat}

case class RecommendationEntry(mal_id: Int, url: UrlString, title: String, images: Images)

object RecommendationEntry {
  implicit val formats: OFormat[RecommendationEntry] = Json.format[RecommendationEntry]
}
