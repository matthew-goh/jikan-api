package models.reviews

import play.api.libs.json.{Json, OFormat}

case class Reviewer(username: String)

object Reviewer {
  implicit val formats: OFormat[Reviewer] = Json.format[Reviewer]
}
