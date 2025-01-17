package controllers

import models._
import models.people.VoicedCharacter
import play.api.mvc._
import play.filters.csrf.CSRF
import services.JikanService

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

@Singleton
class PersonController @Inject()(service: JikanService, val controllerComponents: ControllerComponents)
                                (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def getPersonProfile(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getPersonProfile(id).value.map{
      case Right(personResult) => Ok(views.html.people.personprofile(personResult.data))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def getVoicedCharacters(id: String, role: String, orderBy: String, sortOrder: String): Action[AnyContent] = Action.async { implicit request =>
    val roleTry: Try[CharacterRoles.Value] = Try(CharacterRoles.withName(role))
    val orderByTry: Try[VoicedCharacterOrders.Value] = Try(VoicedCharacterOrders.withName(orderBy))
    val sortOrderTry: Try[SortOrders.Value] = Try(SortOrders.withName(sortOrder))

    (roleTry, orderByTry, sortOrderTry) match {
      case (Success(roleValue), Success(orderByValue), Success(sortOrderValue)) =>
        service.getPersonProfile(id).value.map{
          case Right(personResult) => {
            val sortedVoices: Seq[VoicedCharacter] = personResult.data.voices.filterByRole(roleValue).orderBySortParameter(orderByValue, sortOrderValue)
            Ok(views.html.people.voicedcharacterlist(personResult.data.copy(voices = sortedVoices), role, orderBy, sortOrder))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Invalid sort parameter")))
    }
  }

  def sortVoicedCharacters(id: String): Action[AnyContent] = Action.async { implicit request =>
    val role: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("role").flatMap(_.headOption))
    val orderBy: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("orderBy").flatMap(_.headOption))
    val sortOrder: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("sortOrder").flatMap(_.headOption))
    Future.successful(Redirect(routes.PersonController.getVoicedCharacters(id, role.getOrElse("all"), orderBy.getOrElse("none"), sortOrder.getOrElse("none"))))
  }

  def getAnimePositions(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getPersonProfile(id).value.map{
      case Right(personResult) => Ok(views.html.people.animepositionlist(personResult.data))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }
}
