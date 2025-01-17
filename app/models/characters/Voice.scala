package models.characters

import models.BasicProfileInfo
import play.api.libs.json.{Json, OFormat}

case class Voice(language: String, person: BasicProfileInfo)

object Voice {
  implicit val formats: OFormat[Voice] = Json.format[Voice]
}
