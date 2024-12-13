package models

import play.api.libs.json.{Json, OFormat}

case class Genre(mal_id: Int, name: String)

object Genre {
  implicit val formats: OFormat[Genre] = Json.format[Genre]
}
