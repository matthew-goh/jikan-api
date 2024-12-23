package models.episodes

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class EpisodeFullDetails(mal_id: Int, title: String, duration: Option[Int], aired: Option[Instant],
                              filler: Boolean, recap: Boolean, synopsis: Option[String])

object EpisodeFullDetails {
  implicit val formats: OFormat[EpisodeFullDetails] = Json.format[EpisodeFullDetails]
}
