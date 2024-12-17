package controllers

import models._
import play.api.libs.json._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.{AnimeRepositoryService, JikanService}

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

@Singleton
class ApplicationController @Inject()(repoService: AnimeRepositoryService, service: JikanService, val controllerComponents: ControllerComponents)
                                     (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  def invalidRoute(path: String): Action[AnyContent] = Action.async { _ =>
    Future.successful(NotFound(views.html.pagenotfound(path)))
  }

  ///// METHODS REQUIRING CONNECTOR ONLY /////
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

  def getAnimeById(id: String): Action[AnyContent] = Action.async { _ =>
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


  ///// METHODS REQUIRING REPOSITORY /////
  def listSavedAnime(): Action[AnyContent] = Action.async {implicit request =>
    repoService.index().map{
      case Right(animeList: Seq[SavedAnime]) => Ok(views.html.savedanime(animeList))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }
}
