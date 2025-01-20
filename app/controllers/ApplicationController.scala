package controllers

import cats.data.EitherT
import models._
import models.relations.Relation
import play.api.libs.json._
import play.api.mvc._
import play.filters.csrf.CSRF
import services.{AnimeRepositoryService, JikanService}

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

  /// Anime search ///
  def getAnimeResults(search: String, page: String, queryExt: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeSearchResults(search, page, queryExt).value.flatMap {
      case Right(animeSearchResult) =>
        repoService.index().map{
          case Right(animeInDB) => {
            val queryParams: AnimeSearchParams = service.queryExtToAnimeSearchParams(queryExt) // fills in blanks by default, even if expected parameter is missing
            Ok(views.html.searchanime(animeSearchResult.data, animeSearchResult.pagination, search, queryExt, queryParams, animeInDB.map(_.MALId)))
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def searchAnime(): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val searchTerm: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("search").flatMap(_.headOption))
    searchTerm match {
      case None | Some("") => Future.successful(BadRequest(views.html.unsuccessful("No search term provided")))
      case Some(search) => {
        val searchParamsJson: JsValue = Json.toJson{
          request.body.asFormUrlEncoded.map(_.map{
            case (param, paramValList) => param -> paramValList.headOption
          })
        }
        // check that form has submitted all parameters
        searchParamsJson.validate[AnimeSearchParams] match {
          case JsSuccess(params, _) =>
            Future.successful(Redirect(routes.ApplicationController.getAnimeResults(search = search, page = "1", queryExt = params.formQueryExt)))
          case JsError(_) => Future.successful(BadRequest(views.html.unsuccessful("Invalid search parameters submitted")))
        }
      }
    }
  }

  def getAnimeById(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getImageList(id, ImageListSubjects.anime).value.flatMap{
          case Right(imageList) =>
            // check if the anime has already been saved
            repoService.read(animeResult.data.mal_id).map{
              case Right(_) => Ok(views.html.animedetails(animeResult.data, imageList.data, inDatabase = true))
              case Left(e) => e.httpResponseStatus match {
                case 404 => Ok(views.html.animedetails(animeResult.data, imageList.data, inDatabase = false))
                case _ => Status(e.httpResponseStatus)(views.html.unsuccessful(e.reason))
              }
            }
          case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  /// Anime extra info ///
  def getEpisodeList(animeId: String, page: String): Action[AnyContent] = Action.async { implicit request =>
    // first get anime details (title, total episodes needed), then get episodes
    service.getAnimeById(animeId).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeEpisodes(animeId, page).value.map{
          case Right(episodeResult) =>
            Try(page.toInt) match {
              case Success(pg) if pg > 0 => Ok(views.html.episodelist(animeResult.data, pg, episodeResult.data, episodeResult.pagination))
              case _ => BadRequest(views.html.unsuccessful("API result obtained but page number is not a positive integer"))
            }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getSingleEpisodeDetails(animeId: String, episodeId: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(animeId).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeEpisodeDetails(animeId, episodeId).value.map{
          case Right(episodeResult) => Ok(views.html.episodedetails(animeResult.data, episodeResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getAnimeCharacters(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeCharacters(id).value.map{
          case Right(charResult) => Ok(views.html.characters(animeResult.data, charResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getCharacterProfile(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getCharacterProfile(id).value.flatMap {
      case Right(charResult) =>
        service.getImageList(id, ImageListSubjects.characters).value.map{
          case Right(imageList) => Ok(views.html.characterprofile(charResult.data, imageList.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getAnimeReviews(id: String, page: String, prelim: String, spoilers: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeReviews(id, page, prelim, spoilers).value.map{
          case Right(reviewsResult) => {
            (Try(page.toInt), Try(prelim.toBoolean), Try(spoilers.toBoolean)) match {
              case (Success(pg), Success(prelimBool), Success(spoilersBool)) if pg > 0 =>
                Ok(views.html.reviews(animeResult.data, pg, reviewsResult.data, reviewsResult.pagination, prelimBool, spoilersBool))
              case _ => BadRequest(views.html.unsuccessful("API result obtained but a query parameter is invalid"))
            }
          }
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def filterReviews(id: String): Action[AnyContent] = Action.async { implicit request =>
    accessToken()
    val preliminary: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("preliminary").flatMap(_.headOption))
    val spoilers: Option[String] = request.body.asFormUrlEncoded.flatMap(_.get("spoilers").flatMap(_.headOption))

    preliminary match {
      case None | Some("true") => spoilers match {
        case None | Some("true") =>
          Future.successful(Redirect(routes.ApplicationController.getAnimeReviews(id, "1", preliminary.getOrElse("false"), spoilers.getOrElse("false"))))
        case Some(_) => Future.successful(BadRequest(views.html.unsuccessful("Invalid value submitted for spoilers")))
      }
      case Some(_) => Future.successful(BadRequest(views.html.unsuccessful("Invalid value submitted for preliminary")))
    }
  }

  def getAnimeRecommendations(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeRecommendations(id).value.map{
          case Right(recResult) => Ok(views.html.recommendations(animeResult.data, recResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getAnimeRelations(id: String): Action[AnyContent] = Action.async { implicit request =>
    val result: EitherT[Future, APIError, Result] = for {
      anime <- service.getAnimeById(id)
      themes <- service.getThemeSongs(id)
      relations <- service.getRelatedAnime(id)
    } yield {
      val animeRelations: Seq[Relation] = relations.data.map { relation =>
        Relation(relation.relation, relation.entry.filter(_.`type` == "anime"))
      }.filter(_.entry.nonEmpty)
      Ok(views.html.relations(anime.data, animeRelations, themes.data))
    }

    result.value.map {
      case Right(res) => res
      case Left(error) => // uses the error of the first service call that failed
        Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
    }
  }

  def getAnimeStatistics(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeStatistics(id).value.map{
          case Right(statsResult) => Ok(views.html.animestats(animeResult.data, statsResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getAnimeNews(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeNews(id).value.map{
          case Right(newsResult) => Ok(views.html.animenews(animeResult.data, newsResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }

  def getAnimeStaff(id: String): Action[AnyContent] = Action.async { implicit request =>
    service.getAnimeById(id).value.flatMap{
      case Right(animeResult) =>
        service.getAnimeStaff(id).value.map{
          case Right(staffResult) => Ok(views.html.staff(animeResult.data, staffResult.data))
          case Left(error) => Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason))
        }
      case Left(error) => Future.successful(Status(error.httpResponseStatus)(views.html.unsuccessful(error.reason)))
    }
  }
}
