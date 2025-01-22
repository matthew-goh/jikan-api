package models.producers

import play.api.libs.json.{Json, OFormat}

case class ProducerResult(data: ProducerData)

object ProducerResult {
  implicit val formats: OFormat[ProducerResult] = Json.format[ProducerResult]
}
