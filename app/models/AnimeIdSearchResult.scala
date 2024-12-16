package models

import play.api.libs.json.{Json, OFormat}

case class AnimeIdSearchResult(data: AnimeData)

object AnimeIdSearchResult {
  implicit val formats: OFormat[AnimeIdSearchResult] = Json.format[AnimeIdSearchResult]
}
