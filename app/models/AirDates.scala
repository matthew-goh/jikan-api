package models

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class AirDates(from: Option[Instant], to: Option[Instant])

object AirDates {
  implicit val formats: OFormat[AirDates] = Json.format[AirDates]
}
