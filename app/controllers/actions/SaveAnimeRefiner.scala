package controllers.actions

import controllers.actions.ModifiedRequests._
import models.{APIError, SavedAnime}
import play.api.mvc.Results._
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class SaveAnimeRefiner @Inject()(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[RequestWithUrl, SaveAnimeRequest] {

  override def refine[A](request: RequestWithUrl[A]): Future[Either[Result, SaveAnimeRequest[A]]] = {
    val reqBody: Option[Map[String, Seq[String]]] = request.body.asInstanceOf[AnyContent].asFormUrlEncoded
    val missingError = APIError.BadAPIResponse(400, "Missing required value")
    val invalidTypeError = APIError.BadAPIResponse(400, "Invalid data type")

    // titleEnglish can be missing
    val titleEnglish: Option[String] = reqBody.flatMap(_.get("titleEnglish").flatMap(_.headOption))

    val reqBodyValuesEither: Either[APIError, SavedAnime] = for {
      // if any required value is missing, the result is Left(missingError)
      idStr <- reqBody.flatMap(_.get("id").flatMap(_.headOption)).toRight(missingError)
      title <- reqBody.flatMap(_.get("title").flatMap(_.headOption)).toRight(missingError)
      typ <- reqBody.flatMap(_.get("type").flatMap(_.headOption)).toRight(missingError)
      // if any data type is invalid, the result is Left(invalidTypeError)
      id <- Try(idStr.toInt).toOption.toRight(invalidTypeError)
      // after .toOption, the below will be Some(Some(x)) if a number, Some(None) if missing, or None if invalid
      numEpisodes <- Try(reqBody.flatMap(_.get("numEpisodes").flatMap(_.headOption)).map(_.toInt)).toOption.toRight(invalidTypeError)
      year <- Try(reqBody.flatMap(_.get("year").flatMap(_.headOption)).map(_.toInt)).toOption.toRight(invalidTypeError)
      score <- Try(reqBody.flatMap(_.get("MALScore").flatMap(_.headOption)).map(_.toDouble)).toOption.toRight(invalidTypeError)
    } yield SavedAnime(id, title, titleEnglish, typ, numEpisodes, year, score)

    reqBodyValuesEither match {
      case Right(animeToSave) => Future.successful(Right(SaveAnimeRequest(animeToSave, request.sourceUrl, request.request)))
      case Left(error) => Future.successful(Left(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))))
    }
  }
}
