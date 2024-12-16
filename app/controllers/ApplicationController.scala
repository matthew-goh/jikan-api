package controllers

import models.{APIError, AnimeModel}
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
        Ok(views.html.searchanime(animeResults, searchResult.pagination, search, queryExt))
      }
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

//  def searchAnime(): Action[AnyContent] = Action.async { implicit request =>
//    accessToken()
//    val usernameSubmitted: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("username").flatMap(_.headOption))
//    usernameSubmitted match {
//      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No username provided")))
//      case Some(username) => Future.successful(Redirect(routes.ApplicationController.getUserDetails(username)))
//    }
//  }
}
