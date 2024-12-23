package models

import play.api.libs.json.{Json, OFormat}

case class EpisodeSearchResult(pagination: SimplePagination, data: Seq[AnimeEpisode])

object EpisodeSearchResult {
  implicit val formats: OFormat[EpisodeSearchResult] = Json.format[EpisodeSearchResult]
}
