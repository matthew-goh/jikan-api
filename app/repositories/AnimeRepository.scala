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
) with DataRepositoryTrait {

  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]] = ???

  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]] = ???

  def read(MALId: Int): Future[Either[APIError, SavedAnime]] = ???

  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, UpdateResult]] = ???

  def updateWithValue(MALId: Int, field: Any, newValue: String): Future[Either[APIError, UpdateResult]] = ???

  def delete(MALId: Int): Future[Either[APIError, DeleteResult]] = ???

  def deleteAll(): Future[Either[APIError, DeleteResult]] = ???
}

@ImplementedBy(classOf[AnimeRepository])
trait DataRepositoryTrait {
  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]]
  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]]
  def read(MALId: Int): Future[Either[APIError, SavedAnime]]
  def update(MALId: Int, anime: SavedAnime): Future[Either[APIError, result.UpdateResult]]
//  def updateWithValue(MALId: Int, field: UserModelFields.Value, newValue: String): Future[Either[APIError, result.UpdateResult]]
  def delete(MALId: Int): Future[Either[APIError, result.DeleteResult]]
  def deleteAll(): Future[Either[APIError, result.DeleteResult]]
}
