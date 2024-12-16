package models

import play.api.libs.json.{Json, OFormat}

case class UserProfileResult(data: UserProfile)

object UserProfileResult {
  implicit val formats: OFormat[UserProfileResult] = Json.format[UserProfileResult]
}
