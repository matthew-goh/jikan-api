package models.people

import models.BasicProfileInfo
import play.api.libs.json.{Json, OFormat}

case class Staff(person: BasicProfileInfo, positions: Seq[String])

object Staff {
  implicit val formats: OFormat[Staff] = Json.format[Staff]
}
