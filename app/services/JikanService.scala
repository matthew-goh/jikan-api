package services

import cats.data.EitherT
import connectors.JikanConnector
import models._
import models.characters._
import play.api.libs.json._

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

class JikanService @Inject()(connector: JikanConnector) {
  def getAnimeSearchResults(search: String, page: String, queryExt: String, urlOverride: Option[String] = None)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeSearchResult] = {
    connector.get[AnimeSearchResult](urlOverride.getOrElse(s"https://api.jikan.moe/v4/anime?q=$search&page=$page&$queryExt"))
  }

  def queryExtToAnimeSearchParams(queryExt: String): AnimeSearchParams = {
    val queryParamMap: Map[String, String] =
      queryExt.split("&").map { param =>
        val parts = param.split("=", 2)
        parts(0) -> (if (parts.length > 1) parts(1) else "")
      }.toMap

    AnimeSearchParams(queryParamMap.getOrElse("status", ""), queryParamMap.getOrElse("min_score", ""),
      queryParamMap.getOrElse("max_score", ""), queryParamMap.getOrElse("order_by", ""), queryParamMap.getOrElse("sort", ""))
  }

  def getAnimeById(id: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeIdSearchResult] = {
    connector.get[AnimeIdSearchResult](s"https://api.jikan.moe/v4/anime/$id")
  }

  def getUserProfile(username: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserProfileResult] = {
    connector.get[UserProfileResult](s"https://api.jikan.moe/v4/users/$username/full")
  }

  def getUserFavourites(username: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, UserFavouritesResult] = {
    connector.get[UserFavouritesResult](s"https://api.jikan.moe/v4/users/$username/favorites")
  }

  def getAnimeEpisodes(animeId: String, page: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, EpisodeSearchResult] = {
    connector.get[EpisodeSearchResult](s"https://api.jikan.moe/v4/anime/$animeId/episodes?page=$page")
  }

  def getAnimeEpisodeDetails(animeId: String, episodeId: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, SingleEpisodeResult] = {
    connector.get[SingleEpisodeResult](s"https://api.jikan.moe/v4/anime/$animeId/episodes/$episodeId")
  }

  def getAnimeCharacters(animeId: String)(implicit ec: ExecutionContext): EitherT[Future, APIError, CharacterSearchResult] = {
    connector.get[CharacterSearchResult](s"https://api.jikan.moe/v4/anime/$animeId/characters")
  }
}
