package controllers

import models._
import play.api.libs.json._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.{AnimeRepositoryService, JikanService}

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Singleton
class SavedAnimeController @Inject()(repoService: AnimeRepositoryService, service: JikanService, val controllerComponents: ControllerComponents)
                                    (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {
  // JikanService used in refreshSavedAnime

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  ///// METHODS FOCUSING ON REPOSITORY /////
  def listSavedAnime(compStatus: String, orderBy: String, sortOrder: String): Action[AnyContent] = Action.async { implicit request =>
    val compStatusTry: Try[SavedAnimeStatus.Value] = Try(SavedAnimeStatus.withName(compStatus))
    val orderByTry: Try[SavedAnimeOrders.Value] = Try(SavedAnimeOrders.withName(orderBy))
    val sortOrderTry: Try[SortOrders.Value] = Try(SortOrders.withName(sortOrder))

    (compStatusTry, orderByTry, sortOrderTry) match {
      case (Success(compStatusValue), Success(orderByValue), Success(sortOrderValue)) => {
        repoService.index().map{
          case Right(animeList) => {
            val animeListSorted = animeList.filterByCompStatus(compStatusValue).orderBySortParameter(orderByValue, sortOrderValue)
            Ok(views.html.savedanime(animeListSorted, compStatus, orderBy, sortOrder))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Invalid sort parameter")))
    }
  }

  def sortSavedList(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val compStatus: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("completionStatus").flatMap(_.headOption))
    val orderBy: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("orderBy").flatMap(_.headOption))
    val sortOrder: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("sortOrder").flatMap(_.headOption))
    Future.successful(Redirect(routes.SavedAnimeController.listSavedAnime(compStatus.getOrElse("all"), orderBy.getOrElse("saved_at"), sortOrder.getOrElse("none"))))
  }

  def listSavedAnimeFromTitleSearch(title: String): Action[AnyContent] = Action.async { implicit request =>
    repoService.titleSearch(title).map{
      case Right(animeList) => Ok(views.html.savedanimeTitleSearch(animeList.sortBy(_.title), title))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def searchSavedAnimeByTitle(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val searchedTitle: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("title").flatMap(_.headOption))
    searchedTitle match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No search title submitted")))
      case Some(title) => Future.successful(Redirect(routes.SavedAnimeController.listSavedAnimeFromTitleSearch(title)))
    }
  }

  def viewSavedAnime(id: String): Action[AnyContent] = Action.async { implicit request =>
    Try(id.toInt) match {
      case Success(malId) => {
        repoService.read(malId).map{
          case Right(anime: SavedAnime) => Ok(views.html.savedanimedetails(anime))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }

  }

  def saveAnime(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val sourceUrl: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("url").flatMap(_.headOption))
    sourceUrl match {
      case Some(url) => {
        repoService.create(request.body.asFormUrlEncoded).map{
          case Right(_) => Ok(views.html.confirmation("Anime saved!", Some(url)))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case None => Future.successful(BadRequest(views.html.unsuccessful("Failed to post source url")))
    }
  }

  def refreshSavedAnime(id: String): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val reqBody: Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    val sourceUrl: Option[String] = reqBody.flatMap(_.get("url").flatMap(_.headOption))
    sourceUrl match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("Failed to post source url")))
      case Some(url) =>
        service.getAnimeById(id).value.flatMap {
          case Right(animeResult) =>
            repoService.refresh(reqBody, animeResult.data).map {
              case Right(_) => Ok(views.html.confirmation("Anime details refreshed!", Some(url)))
              case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
            }
          case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
        }
    }
  }

  def showUpdateFormOld(id: String): Action[AnyContent] = Action.async { implicit request =>
    val idTry: Try[Int] = Try(id.toInt)
    idTry match {
      case Success(id) => {
        repoService.read(id).map {
          case Right(anime) => Ok(views.html.updatesavedanime_old(anime))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }
  }

  def updateFormSubmitOld(id: String): Action[AnyContent] = Action.async { implicit request =>
    // accessed via a POST route, so not possible to call with an invalid ID via URL
    accessToken()
    repoService.update(request.body.asFormUrlEncoded).map{
      case Right(_) => Ok(views.html.confirmation("Anime details updated!", Some(s"/saved/$id")))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def showUpdateForm(id: String): Action[AnyContent] = Action.async { implicit request =>
    val idTry: Try[Int] = Try(id.toInt)
    idTry match {
      case Success(id) => {
        repoService.read(id).map {
          case Right(anime) => {
            val formWithDetails = SavedAnime.savedAnimeForm.fill(anime)
            Ok(views.html.updatesavedanime(id, formWithDetails))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }
  }

  def updateFormSubmit(id: String): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val idTry: Try[Int] = Try(id.toInt)
    idTry match {
      case Success(id) => {
        SavedAnime.savedAnimeForm.bindFromRequest().fold(
          formWithErrors => {
            println(formWithErrors.errors)
            Future.successful(BadRequest(views.html.updatesavedanime(id, formWithErrors)))
          },
          formData => {
            repoService.update(id, formData).map{
              case Right(_) => Ok(views.html.confirmation("Anime details updated!", Some(s"/saved/$id")))
              case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
            }
          }
        )
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }
  }

  def unsaveAnime(id: String): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val idTry: Try[Int] = Try(id.toInt)
    val sourceUrl: String = request.body.asFormUrlEncoded.flatMap(_.get("url").flatMap(_.headOption))
      .getOrElse("/saved/status=all/orderby=saved_at/order=none")
    idTry match {
      case Success(id) =>
        repoService.delete(id).map{
          case Right(_) => Ok(views.html.confirmation("Anime removed from saved list", Some(sourceUrl)))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }
  }

  // test-only
  def deleteAll(): Action[AnyContent] = Action.async { _ =>
    repoService.deleteAll().map{
      case Right(deleteResult) => deleteResult.getDeletedCount match {
        case 0 => Ok(views.html.confirmation("No saved anime to delete."))
        case _ => Ok(views.html.confirmation("All saved anime removed from database.")) }
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  ///// NON-FRONTEND METHODS, FOR TESTING /////
  def index(): Action[AnyContent] = Action.async { _ =>
    repoService.index().map{
      case Right(animeList: Seq[SavedAnime]) => Ok {Json.toJson(animeList)}
      case Left(error) => Status(error.httpResponseStatus)(error.reason)
    }
  }

  def create(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body.validate[SavedAnime] match {
      case JsSuccess(anime, _) =>
        repoService.create(anime).map{
          case Right(_) => Created {request.body}
          case Left(error) => Status(error.httpResponseStatus)(error.reason)
        }
      case JsError(_) => Future.successful(BadRequest {"Invalid request body"})
    }
  }
}
