package models.userupdates

import play.api.libs.json.{Json, OFormat}

case class UserUpdatesResult(data: UserUpdatesData)

object UserUpdatesResult {
  implicit val formats: OFormat[UserUpdatesResult] = Json.format[UserUpdatesResult]
}
