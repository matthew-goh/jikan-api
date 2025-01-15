package models.recommendations

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

case class Recommendation(entry: MediaEntry, votes: Int)

object Recommendation {
  implicit val formats: OFormat[Recommendation] = Json.format[Recommendation]
}
