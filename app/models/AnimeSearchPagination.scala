package models

import play.api.libs.json.{Json, OFormat}

case class AnimeSearchPagination(current_page: Int, last_visible_page: Int, has_next_page: Boolean, items: AnimeSearchPagItems)

object AnimeSearchPagination {
  implicit val formats: OFormat[AnimeSearchPagination] = Json.format[AnimeSearchPagination]
}
