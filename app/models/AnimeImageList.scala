package models

import play.api.libs.json.{Json, OFormat}

case class AnimeImageList(data: Seq[Images])

object AnimeImageList {
  implicit val formats: OFormat[AnimeImageList] = Json.format[AnimeImageList]
}
