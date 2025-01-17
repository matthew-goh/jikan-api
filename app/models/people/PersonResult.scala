package models.people

import play.api.libs.json.{Json, OFormat}

case class PersonResult(data: PersonProfile)

object PersonResult {
  implicit val formats: OFormat[PersonResult] = Json.format[PersonResult]
}
