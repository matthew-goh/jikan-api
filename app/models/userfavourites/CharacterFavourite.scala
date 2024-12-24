package models.userfavourites

import models.Images
import play.api.libs.json.{Json, OFormat}

case class CharacterFavourite(mal_id: Int, name: String, images: Images)

object CharacterFavourite {
  implicit val formats: OFormat[CharacterFavourite] = Json.format[CharacterFavourite]
}
