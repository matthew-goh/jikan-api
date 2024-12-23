package models

import play.api.libs.json.{Json, OFormat}

case class UserFavouritesData(anime: Seq[AnimeFavourite], characters: Seq[CharacterFavourite])

object UserFavouritesData {
  implicit val formats: OFormat[UserFavouritesData] = Json.format[UserFavouritesData]
}
