package models

import play.api.libs.json.{Json, OFormat}

case class MediaEntry(mal_id: Int, url: String, title: String, images: Images)

object MediaEntry {
  implicit val formats: OFormat[MediaEntry] = Json.format[MediaEntry]
}
