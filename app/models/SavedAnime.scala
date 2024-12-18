package models

import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class SavedAnime(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int], year: Option[Int], MALScore: Option[Double],
                      savedAt: Instant = Instant.now(), episodesWatched: Int = 0, score: Option[Int] = None, notes: String = "")

object SavedAnime {
  implicit val formats: OFormat[SavedAnime] = Json.format[SavedAnime]
}
