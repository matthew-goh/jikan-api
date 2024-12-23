package models

import models.characters.CharacterImages
import play.api.libs.json.{Json, OFormat}

case class CharacterFavourite(mal_id: Int, name: String, images: CharacterImages)

object CharacterFavourite {
  implicit val formats: OFormat[CharacterFavourite] = Json.format[CharacterFavourite]
}
