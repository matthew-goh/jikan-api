package models

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class AnimeEpisode(mal_id: Int, title: String, aired: Option[Instant], score: Option[Double])

object AnimeEpisode {
  implicit val formats: OFormat[AnimeEpisode] = Json.format[AnimeEpisode]
}
