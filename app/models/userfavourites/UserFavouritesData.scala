package models.userfavourites

import models.BasicProfileInfo
import play.api.libs.json.{Json, OFormat}

case class UserFavouritesData(anime: Seq[AnimeFavourite], characters: Seq[BasicProfileInfo], people: Seq[BasicProfileInfo])

object UserFavouritesData {
  implicit val formats: OFormat[UserFavouritesData] = Json.format[UserFavouritesData]
}
