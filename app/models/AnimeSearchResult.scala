package models

import play.api.libs.json.{Json, OFormat}

case class AnimeSearchResult(pagination: AnimeSearchPagination, data: Seq[AnimeData])

object AnimeSearchResult {
  implicit val formats: OFormat[AnimeSearchResult] = Json.format[AnimeSearchResult]
}
