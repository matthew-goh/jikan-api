package models.recommendations

import models.SimplePagination
import play.api.libs.json.{Json, OFormat}

case class UserPairingResult(pagination: SimplePagination, data: Seq[Pairing])

object UserPairingResult {
  implicit val formats: OFormat[UserPairingResult] = Json.format[UserPairingResult]
}
