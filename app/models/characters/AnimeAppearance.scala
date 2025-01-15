package models.characters

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

case class AnimeAppearance(role: String, anime: MediaEntry)

object AnimeAppearance {
  implicit val formats: OFormat[AnimeAppearance] = Json.format[AnimeAppearance]
}
