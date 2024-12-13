package controllers

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

  def invalidRoute(path: String): Action[AnyContent] = Action.async {_ =>
    Future.successful(NotFound(views.html.pagenotfound(path)))
  }
}
