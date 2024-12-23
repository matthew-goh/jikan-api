package models.episodes

import play.api.libs.json.{Json, OFormat}

case class SingleEpisodeResult(data: EpisodeFullDetails)

object SingleEpisodeResult {
  implicit val formats: OFormat[SingleEpisodeResult] = Json.format[SingleEpisodeResult]
}
