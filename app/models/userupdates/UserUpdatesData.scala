package models.userupdates

import play.api.libs.json.{Json, OFormat}

case class UserUpdatesData(anime: Seq[UserListUpdate])

object UserUpdatesData {
  implicit val formats: OFormat[UserUpdatesData] = Json.format[UserUpdatesData]
}
