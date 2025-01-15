package models

import be.venneborg.refined.play.RefinedJsonFormats._
import models.RefinedTypes.UrlString
import play.api.libs.json.{Json, OFormat}

case class MediaEntry(mal_id: Int, url: UrlString, title: String, images: Images)

object MediaEntry {
  implicit val formats: OFormat[MediaEntry] = Json.format[MediaEntry]
}
