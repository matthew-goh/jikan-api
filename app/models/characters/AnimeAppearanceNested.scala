package models.characters

import play.api.libs.json.{Json, OFormat}

case class AnimeAppearanceNested(mal_id: Int, title: String)

object AnimeAppearanceNested {
  implicit val formats: OFormat[AnimeAppearanceNested] = Json.format[AnimeAppearanceNested]
}
