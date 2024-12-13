package models

import play.api.libs.json.{Json, OFormat}

case class AnimeSearchPagItems(count: Int, total: Int, per_page: Int)

object AnimeSearchPagItems {
  implicit val formats: OFormat[AnimeSearchPagItems] = Json.format[AnimeSearchPagItems]
}
