package controllers

import models.{APIError, AnimeModel, AnimeSearchParams}
import play.api.libs.json._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.JikanService

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ApplicationController @Inject()(service: JikanService, val controllerComponents: ControllerComponents)
                                     (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  def invalidRoute(path: String): Action[AnyContent] = Action.async { _ =>
    Future.successful(NotFound(views.html.pagenotfound(path)))
  }

  def getAnimeResults(search: String, page: Int = 1, queryExt: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeSearchResults(search, page, queryExt).value.map{
      case Right(searchResult) => {
        val animeResults: Seq[AnimeModel] = searchResult.data.map(service.animeDataToModel)
        val queryParams: AnimeSearchParams = service.queryExtToAnimeSearchParams(queryExt)
        Ok(views.html.searchanime(animeResults, searchResult.pagination, search, queryExt, queryParams))
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
        val queryExt = s"status=${status.getOrElse("")}&min_score=${minScore.getOrElse("")}&max_score=${maxScore.getOrElse("")}&order_by=${orderBy.getOrElse("")}&sort=${sortOrder.getOrElse()}"
        Future.successful(Redirect(routes.ApplicationController.getAnimeResults(search = search, page = 1, queryExt = queryExt)))
      }
    }
  }
}
