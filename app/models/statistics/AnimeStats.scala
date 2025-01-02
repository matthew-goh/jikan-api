package models.statistics

import play.api.libs.json.{Json, OFormat}

case class AnimeStats(watching: Int, completed: Int, on_hold: Int, dropped: Int, plan_to_watch: Int, total: Int,
                      scores: Seq[Score])

object AnimeStats {
  implicit val formats: OFormat[AnimeStats] = Json.format[AnimeStats]
}
