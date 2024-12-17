package services

import models.{APIError, SavedAnime}
import org.mongodb.scala.result
import repositories.AnimeRepositoryTrait

import javax.inject.Inject
import scala.concurrent.Future

class AnimeRepositoryService @Inject()(repositoryTrait: AnimeRepositoryTrait){

  def index(): Future[Either[APIError.BadAPIResponse, Seq[SavedAnime]]] = {
    repositoryTrait.index()
  }

  def create(anime: SavedAnime): Future[Either[APIError.BadAPIResponse, SavedAnime]] = {
    repositoryTrait.create(anime)
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
