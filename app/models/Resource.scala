package models

import play.api.libs.json.{Json, OFormat}

case class Resource(mal_id: Int, `type`: String, name: String)

object Resource {
  implicit val formats: OFormat[Resource] = Json.format[Resource]
}
