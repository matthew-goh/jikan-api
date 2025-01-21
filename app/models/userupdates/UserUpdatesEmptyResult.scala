package models.userupdates

import play.api.libs.json.{Json, OFormat}

case class UserUpdatesEmptyResult(data: Seq[UserListUpdate]) extends UserUpdatesAPIResult
// data is an empty list for some users

object UserUpdatesEmptyResult {
  implicit val formats: OFormat[UserUpdatesEmptyResult] = Json.format[UserUpdatesEmptyResult]
}
