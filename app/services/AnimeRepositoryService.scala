package services

import models.{APIError, AnimeData, SavedAnime}
import org.mongodb.scala.result
import repositories.AnimeRepositoryTrait

import javax.inject.Inject
import scala.concurrent.Future
import scala.util.Try

class AnimeRepositoryService @Inject()(repositoryTrait: AnimeRepositoryTrait){

  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]] = {
    repositoryTrait.index()
  }

//    def animeDataToSavedAnime(animeData: AnimeData): SavedAnime = {
//      SavedAnime(MALId = animeData.mal_id,
//        title = animeData.title,
//        titleEnglish = animeData.title_english,
//        `type` = animeData.`type`,
//        numEpisodes = animeData.episodes,
//        year = animeData.year,
//        MALScore = animeData.score,
//      )
//    }

  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]] = {
    repositoryTrait.create(anime)
  }
  // version called from frontend
  def create(reqBody: Option[Map[String, Seq[String]]]): Future[Either[APIError.BadAPIResponse, SavedAnime]] = {
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

  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, result.UpdateResult]] = {
    repositoryTrait.update(MALId, anime)
  }

  def delete(MALId: Int): Future[Either[APIError, result.DeleteResult]] = {
    repositoryTrait.delete(MALId)
  }

  // test-only
  def deleteAll(): Future[Either[APIError, result.DeleteResult]] = {
    repositoryTrait.deleteAll()
  }
}
