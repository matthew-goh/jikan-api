package models

import play.api.libs.json.{Json, OFormat}

import java.util.Date

case class AirDates(from: Option[Date], to: Option[Date])

object AirDates {
  implicit val formats: OFormat[AirDates] = Json.format[AirDates]
}
