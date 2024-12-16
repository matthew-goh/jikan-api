package services

import cats.data.EitherT
import connectors.JikanConnector
import models.{APIError, AnimeData, AnimeModel, AnimeSearchQuery, AnimeSearchResult, Genre}
import play.api.libs.json._

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

class JikanService @Inject()(connector: JikanConnector) {
  def getAnimeSearchResults(search: String, page: Int, queryExt: String, urlOverride: Option[String] = None)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeSearchResult] = {

//    def addToQuery(parameter: String, paramValue: Option[String]): String = paramValue match {
//      case Some(value) => s"&$parameter=$value"
//      case None => ""
//    }

//    val minScoreExt = query.minScore match {
//      case Some(score) => s"&min_score=$score"
//      case None => ""
//    }
//    val maxScoreExt = query.maxScore match {
//      case Some(score) => s"&max_score=$score"
//      case None => ""
//    }
//    val orderByExt = query.orderBy match {
//      case Some(attr) => s"&order_by=$attr"
//    }

    connector.get[AnimeSearchResult](urlOverride.getOrElse(s"https://api.jikan.moe/v4/anime?q=$search&page=$page&$queryExt"))
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
}
