package models

import be.venneborg.refined.play.RefinedJsonFormats._
import eu.timepit.refined.refineMV
import models.RefinedTypes.{NaturalNum, ScoreInt}
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class SavedAnime(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int],
                      year: Option[Int], MALScore: Option[Double], savedAt: Instant = Instant.now(),
                      episodesWatched: NaturalNum = refineMV(0), score: Option[ScoreInt] = None, notes: String = "")

object SavedAnime {
  implicit val formats: OFormat[SavedAnime] = Json.format[SavedAnime]
}
