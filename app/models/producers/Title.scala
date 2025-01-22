package models.producers

import play.api.libs.json.{Json, OFormat}

case class Title(`type`: String, title: String)

object Title {
  implicit val formats: OFormat[Title] = Json.format[Title]
}
