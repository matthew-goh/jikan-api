package models.characters

import play.api.libs.json.{Json, OFormat}

case class VoiceActor(mal_id: Int, name: String)

object VoiceActor {
  implicit val formats: OFormat[VoiceActor] = Json.format[VoiceActor]
}
