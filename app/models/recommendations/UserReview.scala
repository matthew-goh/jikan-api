package models.recommendations

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class UserReview(`type`: String, entry: RecommendationEntry, date: Instant, review: String, score: Int, tags: Seq[String],
                       is_spoiler: Boolean, is_preliminary: Boolean)

object UserReview {
  implicit val formats: OFormat[UserReview] = Json.format[UserReview]
}
