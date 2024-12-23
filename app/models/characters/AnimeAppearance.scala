package models.characters

import play.api.libs.json.{Json, OFormat}

case class AnimeAppearance(role: String, anime: AnimeAppearanceNested)

object AnimeAppearance {
  implicit val formats: OFormat[AnimeAppearance] = Json.format[AnimeAppearance]
}
