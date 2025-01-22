package models.relations

import models.Resource
import play.api.libs.json.{Json, OFormat}

case class Relation(relation: String, entry: Seq[Resource])

object Relation {
  implicit val formats: OFormat[Relation] = Json.format[Relation]
}
