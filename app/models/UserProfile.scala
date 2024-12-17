package models

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class UserProfile(mal_id: Int, username: String, last_online: Instant, joined: Instant, location: Option[String],
                       statistics: UserStatisticsObj)

object UserProfile {
  implicit val formats: OFormat[UserProfile] = Json.format[UserProfile]
}
