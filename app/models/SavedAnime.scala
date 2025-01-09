package models

import be.venneborg.refined.play.RefinedJsonFormats._
import be.venneborg.refined.play.RefinedForms._
import eu.timepit.refined.refineMV
import models.RefinedTypes.{NaturalNum, ScoreInt}
import play.api.data._
import play.api.data.format.Formats._
import play.api.data.Forms._
import play.api.data.format.Formatter
import play.api.libs.json.{Json, OFormat}

import java.time.Instant
import scala.util.{Failure, Success, Try}

case class SavedAnime(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int],
                      year: Option[Int], MALScore: Option[Double], savedAt: Instant = Instant.now(),
                      episodesWatched: NaturalNum = refineMV(0), score: Option[ScoreInt] = None, notes: String = "")

object SavedAnime {
  implicit val formats: OFormat[SavedAnime] = Json.format[SavedAnime]

  implicit val instantFormatter: Formatter[Instant] = new Formatter[Instant] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Instant] = {
      data.get(key)
        .map { value =>
          Try(Instant.parse(value)) match {
            case Success(inst) => Right(inst)
            case Failure(_) => Left(Seq(FormError(key, "error.instant", Nil)))
          }
        }
        .getOrElse(Left(Seq(FormError(key, "error.required", Nil))))
    }

    override def unbind(key: String, value: Instant): Map[String, String] =
      Map(key -> value.toString)
  }

  val savedAnimeForm: Form[SavedAnime] = Form(
    mapping(
      "MALId" -> number,
      "title" -> text,
      "titleEnglish" -> optional(text),
      "type" -> text,
      "numEpisodes" -> optional(number),
      "year" -> optional(number),
      "MALScore" -> optional(of(doubleFormat)),
      "savedAt" -> Forms.of[Instant], // defined custom formatter
      "episodesWatched" -> Forms.of[NaturalNum],
      "score" -> optional(Forms.of[ScoreInt]),
      "notes" -> text
    )(SavedAnime.apply)(SavedAnime.unapply)
  )
}
