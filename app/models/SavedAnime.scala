package models

import play.api.libs.json.{Json, OFormat}

case class SavedAnime(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int],
                      year: Option[Int], MALScore: Double, score: Int, notes: String)

object SavedAnime {
  implicit val formats: OFormat[SavedAnime] = Json.format[SavedAnime]
}
