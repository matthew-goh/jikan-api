package models

import play.api.libs.json.{Json, OFormat}

case class UserAnimeStatistics(days_watched: Double, mean_score: Double, watching: Int, completed: Int,
                               dropped: Int, plan_to_watch: Int, episodes_watched: Int)

object UserAnimeStatistics {
  implicit val formats: OFormat[UserAnimeStatistics] = Json.format[UserAnimeStatistics]
}
