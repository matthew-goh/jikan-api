package models.userprofile

import models.Images
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class UserProfile(mal_id: Int, username: String, last_online: Instant, joined: Instant, location: Option[String],
                       statistics: UserStatisticsObj, images: Images)

object UserProfile {
  implicit val formats: OFormat[UserProfile] = Json.format[UserProfile]
}
