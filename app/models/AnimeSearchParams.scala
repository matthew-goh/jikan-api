package models

import play.api.libs.json.{Json, OFormat}

case class AnimeSearchParams(status: String, minScore: String, maxScore: String, orderBy: String, sort: String)

object AnimeSearchParams {
  implicit val formats: OFormat[AnimeSearchParams] = Json.format[AnimeSearchParams]

  implicit class AnimeSearchParamsOps(params: AnimeSearchParams) {
    def formQueryExt: String = s"status=${params.status}&min_score=${params.minScore}&max_score=${params.maxScore}&order_by=${params.orderBy}&sort=${params.sort}"
  }
}
