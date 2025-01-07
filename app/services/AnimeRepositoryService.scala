package services

import eu.timepit.refined.api._
import models.RefinedTypes.{NaturalNum, ScoreInt}
import models.{APIError, AnimeData, SavedAnime}
import org.mongodb.scala.result
import repositories.AnimeRepositoryTrait

import java.time.Instant
import javax.inject.Inject
import scala.concurrent.Future
import scala.util.Try

class AnimeRepositoryService @Inject()(repositoryTrait: AnimeRepositoryTrait){

  def index(): Future[Either[APIError, Seq[SavedAnime]]] = {
    repositoryTrait.index()
  }

  def create(anime: SavedAnime): Future[Either[APIError, SavedAnime]] = {
    repositoryTrait.create(anime)
  }
  // version called by frontend
  def create(reqBody: Option[Map[String, Seq[String]]]): Future[Either[APIError, SavedAnime]] = {
    val missingError = APIError.BadAPIResponse(400, "Missing required value")
    val invalidTypeError = APIError.BadAPIResponse(400, "Invalid data type")

    // titleEnglish can be missing
    val titleEnglish: Option[String] = reqBody.flatMap(_.get("titleEnglish").flatMap(_.headOption))

    val reqBodyValuesEither: Either[APIError.BadAPIResponse, SavedAnime] = for {
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
      case Right(anime) => repositoryTrait.create(anime)
      case Left(error) => Future.successful(Left(error))
    }
  }

  def read(MALId: Int): Future[Either[APIError, SavedAnime]] = {
    repositoryTrait.read(MALId)
  }

  def titleSearch(search: String): Future[Either[APIError, Seq[SavedAnime]]] = {
    repositoryTrait.titleSearch(search)
  }

//  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, result.UpdateResult]] = {
//    repositoryTrait.update(MALId, anime)
//  }
  // version called by frontend
  def update(reqBody: Option[Map[String, Seq[String]]]): Future[Either[APIError, result.UpdateResult]] = {
    val missingError = APIError.BadAPIResponse(400, "Missing required value")
    val invalidTypeError = APIError.BadAPIResponse(400, "Invalid data type")

    // titleEnglish can be missing
    val titleEnglish: Option[String] = reqBody.flatMap(_.get("titleEnglish").flatMap(_.headOption))
    // notes can be blank
    val notes: String = reqBody.flatMap(_.get("notes").flatMap(_.headOption)).getOrElse("")
    // want score to be None if a blank was submitted
    val myScoreOpt: Option[String] = reqBody.flatMap(_.get("score").flatMap(_.headOption)) match {
      case None | Some("") => None
      case Some(score) => Some(score)
    }

    val reqBodyValuesEither: Either[APIError.BadAPIResponse, SavedAnime] = for {
      // if any required value is missing, the result is Left(missingError)
      idStr <- reqBody.flatMap(_.get("id").flatMap(_.headOption)).toRight(missingError)
      title <- reqBody.flatMap(_.get("title").flatMap(_.headOption)).toRight(missingError)
      typ <- reqBody.flatMap(_.get("type").flatMap(_.headOption)).toRight(missingError)
      savedAtStr <- reqBody.flatMap(_.get("savedAt").flatMap(_.headOption)).toRight(missingError)
      epsWatchedStr <- reqBody.flatMap(_.get("epsWatched").flatMap(_.headOption)).toRight(missingError)
      // if any data type is invalid, the result is Left(invalidTypeError)
      id <- Try(idStr.toInt).toOption.toRight(invalidTypeError)
      savedAt <- Try(Instant.parse(savedAtStr)).toOption.toRight(invalidTypeError)
        // after .toOption, the below will be Some(Some(x)) if a number, Some(None) if missing, or None if invalid
      numEpisodes <- Try(reqBody.flatMap(_.get("numEpisodes").flatMap(_.headOption)).map(_.toInt)).toOption.toRight(invalidTypeError)
      year <- Try(reqBody.flatMap(_.get("year").flatMap(_.headOption)).map(_.toInt)).toOption.toRight(invalidTypeError)
      malScore <- Try(reqBody.flatMap(_.get("MALScore").flatMap(_.headOption)).map(_.toDouble)).toOption.toRight(invalidTypeError)
      // refined types
      epsWatchedInt <- Try(epsWatchedStr.toInt).toOption.toRight(invalidTypeError)
      epsWatched <- RefType.applyRef[NaturalNum](epsWatchedInt).left.map(_ => invalidTypeError)
      myScoreInt <- Try(myScoreOpt.map(_.toInt)).toOption.toRight(invalidTypeError)
      myScore <- myScoreInt match {
        case None => Right(None)
        case Some(x) => RefType.applyRef[ScoreInt](x) match {
          case Right(x) => Right(Some(x))
          case Left(_) => Left(invalidTypeError)
        }
      }
    } yield SavedAnime(id, title, titleEnglish, typ, numEpisodes, year, malScore, savedAt, epsWatched, myScore, notes)

    reqBodyValuesEither match {
      case Right(anime) => repositoryTrait.update(anime.MALId, anime)
      case Left(error) => Future.successful(Left(error))
    }
  }

  def refresh(reqBody: Option[Map[String, Seq[String]]], animeData: AnimeData): Future[Either[APIError, result.UpdateResult]] = {
    val missingError = APIError.BadAPIResponse(400, "Missing required value")
    val invalidTypeError = APIError.BadAPIResponse(400, "Invalid data type")

    val reqBodyValuesEither: Either[APIError.BadAPIResponse, SavedAnime] = for {
      // if any required value is missing, the result is Left(missingError)
      savedAtStr <- reqBody.flatMap(_.get("savedAt").flatMap(_.headOption)).toRight(missingError)
      epsWatchedStr <- reqBody.flatMap(_.get("epsWatched").flatMap(_.headOption)).toRight(missingError)
      notes <- reqBody.flatMap(_.get("notes").flatMap(_.headOption)).toRight(missingError)
      // if any data type is invalid, the result is Left(invalidTypeError)
      savedAt <- Try(Instant.parse(savedAtStr)).toOption.toRight(invalidTypeError)
      // refined types
      epsWatchedInt <- Try(epsWatchedStr.toInt).toOption.toRight(invalidTypeError)
      epsWatched <- RefType.applyRef[NaturalNum](epsWatchedInt).left.map(_ => invalidTypeError)
      myScoreInt <- Try(reqBody.flatMap(_.get("score").flatMap(_.headOption)).map(_.toInt)).toOption.toRight(invalidTypeError)
      myScore <- myScoreInt match {
        case None => Right(None)
        case Some(x) => RefType.applyRef[ScoreInt](x) match {
          case Right(x) => Right(Some(x))
          case Left(_) => Left(invalidTypeError)
        }
      }
    } yield SavedAnime(animeData.mal_id, animeData.title, animeData.title_english, animeData.`type`, animeData.episodes, animeData.year,
      animeData.score, savedAt, epsWatched, myScore, notes)

    reqBodyValuesEither match {
      case Right(animeToSave) => repositoryTrait.update(animeData.mal_id, animeToSave)
      case Left(error) => Future.successful(Left(error))
    }
  }

  def delete(MALId: Int): Future[Either[APIError, result.DeleteResult]] = {
    repositoryTrait.delete(MALId)
  }

  // test-only
  def deleteAll(): Future[Either[APIError, result.DeleteResult]] = {
    repositoryTrait.deleteAll()
  }
}
