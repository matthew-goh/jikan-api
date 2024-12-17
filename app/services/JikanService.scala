package services

import cats.data.EitherT
import connectors.JikanConnector
import models._
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

  def animeDataToModel(animeData: AnimeData): AnimeModel = {
    AnimeModel(MALId = animeData.mal_id,
      title = animeData.title,
      titleEnglish = animeData.title_english,
      `type` = animeData.`type`,
      numEpisodes = animeData.episodes,
      status = animeData.status,
      startDate = animeData.aired.from,
      endDate = animeData.aired.to,
      maturityRating = animeData.rating,
      avgScore = animeData.score,
      scoredBy = animeData.scored_by,
      synopsis = animeData.synopsis,
      genres = animeData.genres.map(genre => genre.name),
      year = animeData.year)
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
}
