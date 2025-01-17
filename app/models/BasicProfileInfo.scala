package models

import play.api.libs.json.{Json, OFormat}

case class BasicProfileInfo(mal_id: Int, name: String, images: Images)

object BasicProfileInfo {
  implicit val formats: OFormat[BasicProfileInfo] = Json.format[BasicProfileInfo]
}
