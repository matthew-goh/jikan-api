package models

import play.api.libs.json.{Json, OFormat}

case class SimplePagination(last_visible_page: Int, has_next_page: Boolean)

object SimplePagination {
  implicit val formats: OFormat[SimplePagination] = Json.format[SimplePagination]
}
