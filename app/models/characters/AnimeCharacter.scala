package models.characters

import models.BasicProfileInfo
import play.api.libs.json.{Json, OFormat}

case class AnimeCharacter(character: BasicProfileInfo, role: String, favorites: Int)

object AnimeCharacter {
  implicit val formats: OFormat[AnimeCharacter] = Json.format[AnimeCharacter]
}
