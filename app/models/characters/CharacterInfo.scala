package models.characters

import models.Images
import play.api.libs.json.{Json, OFormat}

case class CharacterInfo(mal_id: Int, name: String, images: Images)

object CharacterInfo {
  implicit val formats: OFormat[CharacterInfo] = Json.format[CharacterInfo]
}
