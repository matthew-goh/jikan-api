package models.people

import play.api.libs.json.{Json, OFormat}

case class StaffResult(data: Seq[Staff])

object StaffResult {
  implicit val formats: OFormat[StaffResult] = Json.format[StaffResult]
}
