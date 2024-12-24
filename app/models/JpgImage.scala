package models

import play.api.libs.json.{Json, OFormat}

case class JpgImage(image_url: String)

object JpgImage {
  implicit val formats: OFormat[JpgImage] = Json.format[JpgImage]
}
