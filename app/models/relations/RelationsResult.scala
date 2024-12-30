package models.relations

import play.api.libs.json.{Json, OFormat}

case class RelationsResult(data: Seq[Relation])

object RelationsResult {
  implicit val formats: OFormat[RelationsResult] = Json.format[RelationsResult]
}
