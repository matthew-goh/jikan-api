package models.relations

import play.api.libs.json.{Json, OFormat}

case class Relation(relation: String, entry: Seq[RelatedAnime])

object Relation {
  implicit val formats: OFormat[Relation] = Json.format[Relation]
}
