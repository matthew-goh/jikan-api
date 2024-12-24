package models.userfavourites

import play.api.libs.json.{Json, OFormat}

case class UserFavouritesResult(data: UserFavouritesData)

object UserFavouritesResult {
  implicit val formats: OFormat[UserFavouritesResult] = Json.format[UserFavouritesResult]
}
