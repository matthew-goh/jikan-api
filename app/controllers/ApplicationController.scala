package controllers

import models._
import play.api.libs.json._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.{AnimeRepositoryService, JikanService}

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Singleton
class ApplicationController @Inject()(repoService: AnimeRepositoryService, service: JikanService, val controllerComponents: ControllerComponents)
                                     (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  def invalidRoute(path: String): Action[AnyContent] = Action.async { _ =>
    Future.successful(NotFound(views.html.pagenotfound(path)))
  }

  ///// METHODS FOCUSING ON CONNECTOR /////
  def getAnimeResults(search: String, page: String, queryExt: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeSearchResults(search, page, queryExt).value.map{
      case Right(animeSearchResult) => {
        val queryParams: AnimeSearchParams = service.queryExtToAnimeSearchParams(queryExt)
        Ok(views.html.searchanime(animeSearchResult.data, animeSearchResult.pagination, search, queryExt, queryParams))
      }
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def searchAnime(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val searchTerm: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("search").flatMap(_.headOption))
    searchTerm match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No search term provided")))
      case Some(search) => {
        val status: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("status").flatMap(_.headOption))
        val minScore: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("minScore").flatMap(_.headOption))
        val maxScore: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("maxScore").flatMap(_.headOption))
        val orderBy: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("orderBy").flatMap(_.headOption))
        val sortOrder: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("sort").flatMap(_.headOption))
        val queryExt = s"status=${status.getOrElse("")}&min_score=${minScore.getOrElse("")}&max_score=${maxScore.getOrElse("")}&order_by=${orderBy.getOrElse("")}&sort=${sortOrder.getOrElse("")}"
        Future.successful(Redirect(routes.ApplicationController.getAnimeResults(search = search, page = "1", queryExt = queryExt)))
      }
    }
  }

  def getAnimeById(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.map{
      case Right(animeResult) => Ok(views.html.animedetails(animeResult.data))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def getUserProfile(username: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserProfile(username).value.map{
      case Right(userResult) => Ok(views.html.userdetails(userResult.data, username))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def searchUser(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val usernameSubmitted: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("username").flatMap(_.headOption))
    usernameSubmitted match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No username provided")))
      case Some(username) => Future.successful(Redirect(routes.ApplicationController.getUserProfile(username)))
    }
  }

  def getUserFavourites(username: String, orderBy: String = "title", sortOrder: String = "asc"): Action[AnyContent] = Action.async { implicit request =>
    val orderByTry: Try[FavouritesOrders.Value] = Try(FavouritesOrders.withName(orderBy))
    val sortOrderTry: Try[SortOrders.Value] = Try(SortOrders.withName(sortOrder))
    (orderByTry, sortOrderTry) match {
      case (Success(orderByValue), Success(sortOrderValue)) => {
        service.getUserFavourites(username).value.map{
          case Right(favesResult) => {
            val animeFaves: Seq[AnimeFavourite] = favesResult.data.anime
            val animeFavesSorted = orderByValue match {
              case FavouritesOrders.title => sortOrderValue match {
                case SortOrders.desc => animeFaves.sortBy(_.title).reverse
                case _ => animeFaves.sortBy(_.title)
              }
              case FavouritesOrders.start_year => sortOrderValue match {
                case SortOrders.desc => animeFaves.sortBy(_.start_year).reverse
                case _ => animeFaves.sortBy(_.start_year)
              }
              case FavouritesOrders.none => favesResult.data.anime // keep original order
            }
            Ok(views.html.userfavourites(animeFavesSorted, username, orderBy, sortOrder))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Invalid sort parameter")))
    }
  }

  def sortFavourites(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val usernameSubmitted: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("username").flatMap(_.headOption))
    usernameSubmitted match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No username submitted")))
      case Some(username) => {
        val orderBy: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("orderBy").flatMap(_.headOption))
        val sortOrder: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("sortOrder").flatMap(_.headOption))
        Future.successful(Redirect(routes.ApplicationController.getUserFavourites(username, orderBy.getOrElse("none"), sortOrder.getOrElse("none"))))
      }
    }
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
            val animeListFiltered: Seq[SavedAnime] = compStatusValue match {
              case SavedAnimeStatus.not_started => animeList.filter(anime => anime.numEpisodes.isEmpty || anime.episodesWatched == 0)
              case SavedAnimeStatus.watching => animeList.filter(anime => anime.numEpisodes.nonEmpty && anime.episodesWatched > 0 && anime.episodesWatched < anime.numEpisodes.get)
              case SavedAnimeStatus.completed => animeList.filter(anime => anime.numEpisodes.nonEmpty && anime.episodesWatched == anime.numEpisodes.get)
              case _ => animeList
            }
            val animeListSorted = orderByValue match {
              case SavedAnimeOrders.saved_at => sortOrderValue match {
                case SortOrders.desc => animeListFiltered.sortBy(_.savedAt).reverse
                case _ => animeListFiltered.sortBy(_.savedAt)
              }
              case SavedAnimeOrders.title => sortOrderValue match {
                case SortOrders.desc => animeListFiltered.sortBy(_.title).reverse
                case _ => animeListFiltered.sortBy(_.title)
              }
              case SavedAnimeOrders.year => sortOrderValue match {
                case SortOrders.desc => animeListFiltered.sortBy(_.year).reverse
                case _ => animeListFiltered.sortBy(_.year)
              }
              case SavedAnimeOrders.score => sortOrderValue match {
                case SortOrders.desc => animeListFiltered.sortBy(_.score).reverse
                case _ => animeListFiltered.sortBy(_.score)
              }
            }
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
    Future.successful(Redirect(routes.ApplicationController.listSavedAnime(compStatus.getOrElse("all"), orderBy.getOrElse("saved_at"), sortOrder.getOrElse("none"))))
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
      case Some(title) => Future.successful(Redirect(routes.ApplicationController.listSavedAnimeFromTitleSearch(title)))
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

  def refreshSavedAnime(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val reqBody: Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    val sourceUrl: Option[String] = reqBody.flatMap(_.get("url").flatMap(_.headOption))
    val idTry: Try[Int] = Try(reqBody.flatMap(_.get("id").flatMap(_.headOption)).get.toInt)
    (sourceUrl, idTry) match {
      case (Some(url), Success(id)) => {
        service.getAnimeById(id.toString).value.flatMap{
          case Right(animeResult) => {
            repoService.refresh(reqBody, animeResult.data).map{
              case Right(_) => Ok(views.html.confirmation("Anime details refreshed!", Some(url)))
              case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
            }
          }
          case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
        }
      }
      case(Some(_), Failure(_)) => Future.successful(BadRequest(views.html.unsuccessful("Invalid or missing anime ID")))
      case (None, _) => Future.successful(BadRequest(views.html.unsuccessful("Failed to post source url")))
    }
  }

  def showUpdateForm(id: String): Action[AnyContent] = Action.async { implicit request =>
    val idTry: Try[Int] = Try(id.toInt)
    idTry match {
      case Success(id) => {
        repoService.read(id).map {
          case Right(anime) => Ok(views.html.updatesavedanime(anime))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Anime ID must be an integer")))
    }
  }

  def updateFormSubmit(id: String): Action[AnyContent] =  Action.async { implicit request =>
    // accessed via a POST route, so not possible to call with an invalid ID via URL
    accessToken()
    repoService.update(request.body.asFormUrlEncoded).map{
      case Right(_) => Ok(views.html.confirmation("Anime details updated!", Some(s"/saved/$id")))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def unsaveAnime(id: String): Action[AnyContent] = Action.async { implicit request =>
    val idTry: Try[Int] = Try(id.toInt)
    idTry match {
      case Success(id) => {
        repoService.delete(id).map{
          case Right(_) =>
            Ok(views.html.confirmation("Anime removed from saved list", Some("/saved/status=all/orderby=saved_at/order=none")))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
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
