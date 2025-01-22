package models.producers

import models.Images
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class ProducerData(mal_id: Int, titles: Seq[Title], images: Images, favorites: Int, count: Int,
                        established: Option[Instant], about: Option[String])

object ProducerData {
  implicit val formats: OFormat[ProducerData] = Json.format[ProducerData]
}
