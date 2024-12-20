package models

import play.api.libs.json.{Json, OFormat}

case class EpisodePagination(last_visible_page: Int, has_next_page: Boolean)

object EpisodePagination {
  implicit val formats: OFormat[EpisodePagination] = Json.format[EpisodePagination]
}
