package models.people

import models.MediaEntry
import models.characters.CharacterInfo
import play.api.libs.json.{Json, OFormat}

case class VoicedCharacter(role: String, anime: MediaEntry, character: CharacterInfo)

object VoicedCharacter {
  implicit val formats: OFormat[VoicedCharacter] = Json.format[VoicedCharacter]
}
