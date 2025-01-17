package models.people

import models.Images
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class PersonProfile(mal_id: Int, images: Images, name: String, alternate_names: Seq[String], birthday: Option[Instant],
                         favorites: Int, about: Option[String], anime: Seq[AnimePosition], voices: Seq[VoicedCharacter])

object PersonProfile {
  implicit val formats: OFormat[PersonProfile] = Json.format[PersonProfile]
}
