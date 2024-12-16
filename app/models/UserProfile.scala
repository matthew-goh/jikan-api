package models

import play.api.libs.json.{Json, OFormat}

import java.time.Instant
import java.util.Date

case class UserProfile(mal_id: Int, username: String, last_online: Instant, joined: Date, location: Option[String],
                       statistics: UserStatisticsObj)

object UserProfile {
  implicit val formats: OFormat[UserProfile] = Json.format[UserProfile]
}
