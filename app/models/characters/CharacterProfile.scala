package models.characters

import models.Images
import play.api.libs.json.{Json, OFormat}

case class CharacterProfile(mal_id: Int, images: Images, name: String, nicknames: Seq[String], favorites: Int,
                            about: Option[String], anime: Seq[AnimeAppearance], voices: Seq[Voice])

object CharacterProfile {
  implicit val formats: OFormat[CharacterProfile] = Json.format[CharacterProfile]
}
