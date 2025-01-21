package models.userupdates

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class UserListUpdate(entry: MediaEntry, score: Option[Int], status: String,
                          episodes_seen: Option[Int], episodes_total: Option[Int], date: Instant)

object UserListUpdate {
  implicit val formats: OFormat[UserListUpdate] = Json.format[UserListUpdate]
}
