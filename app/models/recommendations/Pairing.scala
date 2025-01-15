package models.recommendations

import models.MediaEntry
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class Pairing(entry: Seq[MediaEntry], content: String, date: Instant)

object Pairing {
  implicit val formats: OFormat[Pairing] = Json.format[Pairing]
}
