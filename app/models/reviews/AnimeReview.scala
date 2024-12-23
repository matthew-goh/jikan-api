package models.reviews

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class AnimeReview(user: Reviewer, date: Instant, review: String, score: Int, tags: Seq[String],
                       is_spoiler: Boolean, is_preliminary: Boolean)

object AnimeReview {
  implicit val formats: OFormat[AnimeReview] = Json.format[AnimeReview]
}
