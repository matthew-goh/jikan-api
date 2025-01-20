package models

import play.api.libs.json.{Json, OFormat}

case class ImageList(data: Seq[Images])

object ImageList {
  implicit val formats: OFormat[ImageList] = Json.format[ImageList]
}
