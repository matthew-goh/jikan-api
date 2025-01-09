package models.userprofile

import play.api.libs.json.{Json, OFormat}

case class UserStatisticsObj(anime: UserAnimeStatistics)

object UserStatisticsObj {
  implicit val formats: OFormat[UserStatisticsObj] = Json.format[UserStatisticsObj]
}
