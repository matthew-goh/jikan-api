package controllers

import models._
import models.userfavourites.AnimeFavourite
import play.api.mvc._
import play.filters.csrf.CSRF
import services.JikanService

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

@Singleton
class UserProfileController @Inject()(service: JikanService, val controllerComponents: ControllerComponents)
                                     (implicit ec: ExecutionContext) extends BaseController with play.api.i18n.I18nSupport {

  def accessToken()(implicit request: Request[_]): Option[CSRF.Token] = {
    CSRF.getToken
  }

  def getUserProfile(username: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserProfile(username).value.map{
      case Right(userResult) => Ok(views.html.userprofile.userdetails(userResult.data, username))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def searchUser(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val usernameSubmitted: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("username").flatMap(_.headOption))
    usernameSubmitted match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No username provided")))
      case Some(username) => Future.successful(Redirect(routes.UserProfileController.getUserProfile(username)))
    }
  }

  def getUserFavouriteAnime(username: String, orderBy: String, sortOrder: String): Action[AnyContent] = Action.async { implicit request =>
    val orderByTry: Try[FavouritesOrders.Value] = Try(FavouritesOrders.withName(orderBy))
    val sortOrderTry: Try[SortOrders.Value] = Try(SortOrders.withName(sortOrder))
    (orderByTry, sortOrderTry) match {
      case (Success(orderByValue), Success(sortOrderValue)) => {
        service.getUserFavourites(username).value.map{
          case Right(favesResult) => {
            val animeFavesSorted: Seq[AnimeFavourite] = favesResult.data.anime.orderBySortParameter(orderByValue, sortOrderValue)
            Ok(views.html.userprofile.userfavouriteanime(animeFavesSorted, username, orderBy, sortOrder))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      }
      case _ => Future.successful(BadRequest(views.html.unsuccessful("Invalid sort parameter")))
    }
  }

  def sortFavourites(username: String): Action[AnyContent] = Action.async { implicit request =>
    val orderBy: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("orderBy").flatMap(_.headOption))
    val sortOrder: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("sortOrder").flatMap(_.headOption))
    Future.successful(Redirect(routes.UserProfileController.getUserFavouriteAnime(username, orderBy.getOrElse("none"), sortOrder.getOrElse("none"))))
  }

  def getUserFavouriteCharacters(username: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserFavourites(username).value.map{
      case Right(favesResult) => Ok(views.html.userprofile.userfavouritecharacters(favesResult.data.characters.sortBy(_.name), username))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def getUserRecommendedPairings(username: String, page: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserRecommendations(username, page).value.map{
      case Right(pairingsResult) => {
        if (pairingsResult.data.exists(pairing => pairing.entry.length != 2))
          InternalServerError(views.html.unsuccessful("Error: An entry returned by the API does not contain a pair"))
        else Try(page.toInt) match {
          case Success(pg) if pg > 0 => Ok(views.html.userprofile.userpairings(pairingsResult.data, username, pg, pairingsResult.pagination))
          case _ => BadRequest(views.html.unsuccessful("API result obtained but page number is not a positive integer"))
        }
      }
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def getUserReviews(username: String, page: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserReviews(username, page).value.flatMap{
      case Right(reviewsResult) => {
        Try(page.toInt) match {
          case Success(pg) if pg > 0 =>
            // has_next_page from API is not accurate - need to fetch next page's data to see if there are any results
            if (reviewsResult.data.isEmpty) Future.successful(Ok(views.html.userprofile.userreviews(username, pg, reviewsResult.data, hasNextPage = false)))
            else service.getUserReviews(username, (pg + 1).toString).value.map{
              case Right(reviewsNextPage) =>
                if (reviewsNextPage.data.isEmpty) Ok(views.html.userprofile.userreviews(username, pg, reviewsResult.data, hasNextPage = false))
                else Ok(views.html.userprofile.userreviews(username, pg, reviewsResult.data, hasNextPage = true))
              case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
            }
          case _ => Future.successful(BadRequest(views.html.unsuccessful("API result obtained but page number is not a positive integer")))
        }
      }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getUserUpdates(username: String): Action[AnyContent] = Action.async { implicit request =>
    service.getUserUpdates(username).value.map{
      case Right(updatesResult) => Ok(views.html.userprofile.userupdates(updatesResult.data.anime, username))
      case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }
}
