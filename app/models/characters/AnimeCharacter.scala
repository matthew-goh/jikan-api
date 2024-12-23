package models.characters

import play.api.libs.json.{Json, OFormat}

case class AnimeCharacter(character: CharacterProfile, role: String, favorites: Int)

object AnimeCharacter {
  implicit val formats: OFormat[AnimeCharacter] = Json.format[AnimeCharacter]
}
