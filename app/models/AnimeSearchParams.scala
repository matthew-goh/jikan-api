package models

import play.api.libs.json.{Json, OFormat}

case class AnimeSearchParams(status: String, minScore: String, maxScore: String, orderBy: String, sort: String)

object AnimeSearchParams {
  implicit val formats: OFormat[AnimeSearchParams] = Json.format[AnimeSearchParams]
}
