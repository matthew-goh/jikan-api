package repositories

import com.google.inject.ImplementedBy
import models.{APIError, SavedAnime}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters.empty
import org.mongodb.scala.model._
import org.mongodb.scala.result.{DeleteResult, UpdateResult}
import org.mongodb.scala.{MongoWriteException, result}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AnimeRepository @Inject()(mongoComponent: MongoComponent)
                               (implicit ec: ExecutionContext) extends PlayMongoRepository[SavedAnime](
  // required parameters for the PlayMongoRepository abstract class
  collectionName = "animelist",
  mongoComponent = mongoComponent,
  domainFormat = SavedAnime.formats, // uses the implicit val formats
  indexes = Seq(IndexModel(
    Indexes.ascending("MALId"),
    IndexOptions().unique(true)  // Ensures the index is unique
  )),
  replaceIndexes = false
) with AnimeRepositoryTrait {

  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]] = {
    collection.find().toFuture().map{ animeList: Seq[SavedAnime] => Right(animeList) }
      .recover{
        case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to search database collection: ${e.getMessage}"))
      }
  }

  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]] = {
    collection.insertOne(anime).toFuture().map { insertResult =>
      if (insertResult.wasAcknowledged) {
        Right(anime)
      } else {
        Left(APIError.BadAPIResponse(500, "Error: Insertion not acknowledged"))
      }
    }.recover {
      case e: MongoWriteException => Left(APIError.BadAPIResponse(500, "Anime has already been saved"))
      case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to save anime: ${e.getMessage}"))
    }
  }

  private def byID(id: Int): Bson =
    Filters.and(
      Filters.equal("MALId", id)
    )
  private def bySearchedTitle(search: String): Bson =
    Filters.and(
      Filters.regex("title", s".*${search}.*", "i")
    )

  def read(MALId: Int): Future[Either[APIError, SavedAnime]] = {
    collection.find(byID(MALId)).headOption.flatMap {
      case Some(data) => Future(Right(data))
      case None => Future(Left(APIError.BadAPIResponse(404, "Anime not saved")))
    }.recover {
      case e: Exception => Left(APIError.BadAPIResponse(500, s"Unable to search for anime: ${e.getMessage}"))
    }
  }

  def titleSearch(search: String): Future[Either[APIError, Seq[SavedAnime]]] = {
    collection.find(bySearchedTitle(search)).toFuture().map(animeList => Right(animeList))
      .recover {
        case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to search for anime: ${e.getMessage}"))
      }
  }

  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, UpdateResult]] = {
    collection.replaceOne(
      filter = byID(MALId),
      replacement = anime,
      options = new ReplaceOptions().upsert(false) // don't add to database if not already there
    ).toFuture().map {
      updateResult =>
        if (updateResult.wasAcknowledged) {
          updateResult.getMatchedCount match {
            case 1 => Right(updateResult)
            case 0 => Left(APIError.BadAPIResponse(404, "Anime not saved"))
            case _ => Left(APIError.BadAPIResponse(500, "Error: Multiple anime with same ID found"))
          }
        } else {
          Left(APIError.BadAPIResponse(500, "Error: Update not acknowledged"))
        }
    }.recover {
      case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to update anime details: ${e.getMessage}"))
    }
  }
  // updateResult is e.g. AcknowledgedUpdateResult{matchedCount=1, modifiedCount=1, upsertedId=null}

//  def updateWithValue(MALId: Int, field: CustomisableFields.Value, newValue: String): Future[Either[APIError, UpdateResult]] = ???

  def delete(MALId: Int): Future[Either[APIError, DeleteResult]] = {
    collection.deleteOne(
      filter = byID(MALId)
    ).toFuture().map { deleteResult =>
      if (deleteResult.wasAcknowledged) {
        deleteResult.getDeletedCount match {
          case 1 => Right(deleteResult)
          case 0 => Left(APIError.BadAPIResponse(404, "Anime not saved"))
          case _ => Left(APIError.BadAPIResponse(500, "Error: Multiple anime removed"))
        }
      } else {
        Left(APIError.BadAPIResponse(500, "Error: Delete not acknowledged"))
      }
    }.recover {
      case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to remove anime: ${e.getMessage}"))
    }
  }

  // test-only
  def deleteAll(): Future[Either[APIError, DeleteResult]] = {
    collection.deleteMany(empty()).toFuture().map{ deleteResult =>
      if (deleteResult.wasAcknowledged) Right(deleteResult)
      else Left(APIError.BadAPIResponse(500, "Error: Delete not acknowledged"))
    }.recover {
      case e: Throwable => Left(APIError.BadAPIResponse(500, s"Unable to remove all anime: ${e.getMessage}"))
    }
  }
}

@ImplementedBy(classOf[AnimeRepository])
trait AnimeRepositoryTrait {
  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]]
  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]]
  def read(MALId: Int): Future[Either[APIError, SavedAnime]]
  def titleSearch(search: String): Future[Either[APIError, Seq[SavedAnime]]]
  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, result.UpdateResult]]
//  def updateWithValue(MALId: Int, field: CustomisableFields.Value, newValue: String): Future[Either[APIError, result.UpdateResult]]
  def delete(MALId: Int): Future[Either[APIError, result.DeleteResult]]
  def deleteAll(): Future[Either[APIError, result.DeleteResult]]
}

//object CustomisableFields extends Enumeration {
//  val watched, score, notes = Value
//}
