package models.recommendations

import models.SimplePagination
import play.api.libs.json.{Json, OFormat}

case class UserReviewsResult(data: Seq[UserReview], pagination: SimplePagination)

object UserReviewsResult {
  implicit val formats: OFormat[UserReviewsResult] = Json.format[UserReviewsResult]
}
