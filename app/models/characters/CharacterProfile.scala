package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterProfile(mal_id: Int, images: CharacterImages, name: String, nicknames: Seq[String], favorites: Int,
                            about: Option[String], anime: Seq[AnimeAppearance])

object CharacterProfile {
  implicit val formats: OFormat[CharacterProfile] = Json.format[CharacterProfile]
}
