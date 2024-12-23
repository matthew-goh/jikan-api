package models

import play.api.libs.json.{Json, OFormat}

case class EpisodeSearchResult(pagination: EpisodePagination, data: Seq[AnimeEpisode])

object EpisodeSearchResult {
  implicit val formats: OFormat[EpisodeSearchResult] = Json.format[EpisodeSearchResult]
}
