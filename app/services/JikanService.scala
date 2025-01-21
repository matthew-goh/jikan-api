package services

import cats.data.EitherT
import connectors.JikanConnector
import models._
import models.characters._
import models.episodes.{EpisodeSearchResult, SingleEpisodeResult}
import models.news.NewsResult
import models.people.{PersonResult, StaffResult}
import models.recommendations._
import models.relations.{RelationsResult, ThemesResult}
import models.reviews.ReviewsResult
import models.statistics.StatisticsResult
import models.userfavourites.UserFavouritesResult
import models.userprofile.UserProfileResult
import models.userupdates._

import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

class JikanService @Inject()(connector: JikanConnector) {
  def getAnimeSearchResults(search: String, page: String, queryExt: String, urlOverride: Option[String] = None)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeSearchResult] = {
    connector.get[AnimeSearchResult](urlOverride.getOrElse(s"https://api.jikan.moe/v4/anime?q=$search&page=$page&$queryExt"))
  }

  def queryExtToAnimeSearchParams(queryExt: String): AnimeSearchParams = {
    val queryParamMap: Map[String, String] = {
      // e.g. List("status=complete", "min_score=7", "max_score=", "order_by=title", "sort=")
      queryExt.split("&").map { param =>
        val parts = param.split("=", 2)
        parts(0) -> (if (parts.length > 1) parts(1) else "")
      }.toMap
    }

    AnimeSearchParams(queryParamMap.getOrElse("status", ""), queryParamMap.getOrElse("min_score", ""),
      queryParamMap.getOrElse("max_score", ""), queryParamMap.getOrElse("order_by", ""), queryParamMap.getOrElse("sort", ""))
  }

  def getAnimeById(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeIdSearchResult] = {
    connector.get[AnimeIdSearchResult](s"https://api.jikan.moe/v4/anime/$id")
  }

  def getImageList(id: String, subject: ImageListSubjects.Value)(implicit ec: ExecutionContext): EitherT[Future, APIError, ImageList] = {
    connector.get[ImageList](s"https://api.jikan.moe/v4/$subject/$id/pictures")
  }

  // User profiles
  def getUserProfile(username: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserProfileResult] = {
    connector.get[UserProfileResult](s"https://api.jikan.moe/v4/users/$username/full")
  }

  def getUserFavourites(username: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserFavouritesResult] = {
    connector.get[UserFavouritesResult](s"https://api.jikan.moe/v4/users/$username/favorites")
  }

  def getUserRecommendations(username: String, page: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserPairingResult] = {
    connector.get[UserPairingResult](s"https://api.jikan.moe/v4/users/$username/recommendations?page=$page")
  }

  def getUserReviews(username: String, page: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserReviewsResult] = {
    connector.get[UserReviewsResult](s"https://api.jikan.moe/v4/users/$username/reviews?page=$page")
  }

  def getUserUpdates(username: String)(implicit ec: ExecutionContext): Future[Either[APIError, UserUpdatesAPIResult]] = {
    connector.get[UserUpdatesResult](s"https://api.jikan.moe/v4/users/$username/userupdates").value.flatMap {
      case Right(updatesResult) => Future.successful(Right(updatesResult))
      case Left(e) => e.httpResponseStatus match {
        case 400 | 404 => Future.successful(Left(e))
        case _ => connector.get[UserUpdatesEmptyResult](s"https://api.jikan.moe/v4/users/$username/userupdates").value
      }
    }
  }

  // Anime extra info
  def getAnimeEpisodes(animeId: String, page: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, EpisodeSearchResult] = {
    connector.get[EpisodeSearchResult](s"https://api.jikan.moe/v4/anime/$animeId/episodes?page=$page")
  }

  def getAnimeEpisodeDetails(animeId: String, episodeId: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, SingleEpisodeResult] = {
    connector.get[SingleEpisodeResult](s"https://api.jikan.moe/v4/anime/$animeId/episodes/$episodeId")
  }

  def getAnimeCharacters(animeId: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, CharacterSearchResult] = {
    connector.get[CharacterSearchResult](s"https://api.jikan.moe/v4/anime/$animeId/characters")
  }

  def getCharacterProfile(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, CharacterProfileResult] = {
    connector.get[CharacterProfileResult](s"https://api.jikan.moe/v4/characters/$id/full")
  }

  def getAnimeReviews(id: String, page: String, prelim: String, spoilers: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, ReviewsResult] = {
    connector.get[ReviewsResult](s"https://api.jikan.moe/v4/anime/$id/reviews?page=$page&preliminary=$prelim&spoilers=$spoilers")
  }

  def getAnimeRecommendations(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, RecommendationsResult] = {
    connector.get[RecommendationsResult](s"https://api.jikan.moe/v4/anime/$id/recommendations")
  }

  def getRelatedAnime(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, RelationsResult] = {
    connector.get[RelationsResult](s"https://api.jikan.moe/v4/anime/$id/relations")
  }

  def getThemeSongs(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, ThemesResult] = {
    connector.get[ThemesResult](s"https://api.jikan.moe/v4/anime/$id/themes")
  }

  def getAnimeStatistics(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, StatisticsResult] = {
    connector.get[StatisticsResult](s"https://api.jikan.moe/v4/anime/$id/statistics")
  }

  def getAnimeNews(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, NewsResult] = {
    connector.get[NewsResult](s"https://api.jikan.moe/v4/anime/$id/news")
  }

  def getAnimeStaff(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, StaffResult] = {
    connector.get[StaffResult](s"https://api.jikan.moe/v4/anime/$id/staff")
  }

  // People
  def getPersonProfile(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, PersonResult] = {
    connector.get[PersonResult](s"https://api.jikan.moe/v4/people/$id/full")
  }
}
