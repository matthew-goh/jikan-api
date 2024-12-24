package models

import play.api.libs.json.{Json, OFormat}

case class Images(jpg: JpgImage)

object Images {
  implicit val formats: OFormat[Images] = Json.format[Images]
}
