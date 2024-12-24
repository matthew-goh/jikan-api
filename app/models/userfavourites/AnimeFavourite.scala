package models.userfavourites

import models.Images
import play.api.libs.json.{Json, OFormat}

case class AnimeFavourite(mal_id: Int, title: String, `type`: String, start_year: Int, images: Images)

object AnimeFavourite {
  implicit val formats: OFormat[AnimeFavourite] = Json.format[AnimeFavourite]
}
