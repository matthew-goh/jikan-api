package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterProfile(mal_id: Int, name: String, images: CharacterImages)

object CharacterProfile {
  implicit val formats: OFormat[CharacterProfile] = Json.format[CharacterProfile]
}
