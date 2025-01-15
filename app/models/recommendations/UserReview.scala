package models.recommendations

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class UserReview(`type`: String, entry: MediaEntry, date: Instant, review: String, score: Int, tags: Seq[String],
                      is_spoiler: Boolean, is_preliminary: Boolean)

object UserReview {
  implicit val formats: OFormat[UserReview] = Json.format[UserReview]
}
