package models.recommendations

import models.SimplePagination
import play.api.libs.json.{Json, OFormat}

case class UserPairingResult(data: Seq[Pairing], pagination: SimplePagination)

object UserPairingResult {
  implicit val formats: OFormat[UserPairingResult] = Json.format[UserPairingResult]
}
