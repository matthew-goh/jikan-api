package models

import play.api.libs.json.{Json, OFormat}

case class AnimeData(mal_id: Int, title: String, title_english: Option[String], title_synonyms: Seq[String], `type`: String, episodes: Option[Int],
                     status: String, aired: AirDates, rating: Option[String], score: Option[Double], scored_by: Option[Int], favorites: Option[Int],
                     synopsis: Option[String], genres: Seq[Genre], year: Option[Int], images: Images)

object AnimeData {
  implicit val formats: OFormat[AnimeData] = Json.format[AnimeData]
}
