package models

import be.venneborg.refined.play.RefinedJsonFormats._
import models.RefinedTypes.UrlString
import play.api.libs.json.{Json, OFormat}

case class JpgImage(image_url: Option[UrlString])

object JpgImage {
  implicit val formats: OFormat[JpgImage] = Json.format[JpgImage]
}
