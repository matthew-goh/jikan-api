package models.characters

import play.api.libs.json.{Json, OFormat}

case class Voice(language: String, person: VoiceActor)

object Voice {
  implicit val formats: OFormat[Voice] = Json.format[Voice]
}
