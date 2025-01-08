package models.news

import play.api.libs.json.{Json, OFormat}

case class NewsResult(data: Seq[AnimeNews])

object NewsResult {
  implicit val formats: OFormat[NewsResult] = Json.format[NewsResult]
}
