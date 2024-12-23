package models.reviews

import models.SimplePagination
import play.api.libs.json.{Json, OFormat}

case class ReviewsResult(data: Seq[AnimeReview], pagination: SimplePagination)

object ReviewsResult {
  implicit val formats: OFormat[ReviewsResult] = Json.format[ReviewsResult]
}
