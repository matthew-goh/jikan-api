package controllers

import models._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.JikanService

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

@Singleton
class PersonController @Inject()(service: JikanService, val controllerComponents: ControllerComponents)
                                (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  def getPersonProfile(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getPersonProfile(id).value.map{
      case Right(personResult) => Ok(views.html.personprofile(personResult.data))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }
}
