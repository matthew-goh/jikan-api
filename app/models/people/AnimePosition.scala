package models.people

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

case class AnimePosition(position: String, anime: MediaEntry)

object AnimePosition {
  implicit val formats: OFormat[AnimePosition] = Json.format[AnimePosition]
}
