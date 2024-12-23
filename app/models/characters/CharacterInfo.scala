package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterInfo(mal_id: Int, name: String, images: CharacterImages)

object CharacterInfo {
  implicit val formats: OFormat[CharacterInfo] = Json.format[CharacterInfo]
}
