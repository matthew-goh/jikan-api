package models.relations

import play.api.libs.json.{Json, OFormat}

case class RelatedAnime(mal_id: Int, `type`: String, name: String)

object RelatedAnime {
  implicit val formats: OFormat[RelatedAnime] = Json.format[RelatedAnime]
}
