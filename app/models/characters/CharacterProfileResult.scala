package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterProfileResult(data: CharacterProfile)

object CharacterProfileResult {
  implicit val formats: OFormat[CharacterProfileResult] = Json.format[CharacterProfileResult]
}
