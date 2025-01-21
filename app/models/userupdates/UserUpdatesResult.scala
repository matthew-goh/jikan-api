package models.userupdates

import play.api.libs.json.{Json, OFormat}

case class UserUpdatesResult(data: UserUpdatesData) extends UserUpdatesAPIResult

object UserUpdatesResult {
  implicit val formats: OFormat[UserUpdatesResult] = Json.format[UserUpdatesResult]
}
