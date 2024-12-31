package models.recommendations

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class Pairing(entry: Seq[RecommendationEntry], content: String, date: Instant)

object Pairing {
  implicit val formats: OFormat[Pairing] = Json.format[Pairing]
}
