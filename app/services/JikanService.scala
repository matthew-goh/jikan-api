package services

import cats.data.EitherT
import connectors.JikanConnector
import models.{APIError, AnimeSearchResult}
import play.api.libs.json._

import java.util.Base64
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

class JikanService @Inject()(connector: JikanConnector) {
  def getAnimeSearchResults(search: String, minScore: Option[String] = None, maxScore: Option[String] = None,
                            orderBy: Option[String] = None, page: Int = 1)(implicit ec: ExecutionContext): EitherT[Future, APIError, AnimeSearchResult] = {
    val minScoreExt = minScore match {
      case Some(score) => s"&min_score=$score"
      case None => ""
    }
    val maxScoreExt = maxScore match {
      case Some(score) => s"&max_score=$score"
      case None => ""
    }
    val orderByExt = orderBy match {
      case Some(attr) => s"&order_by=$attr"
      case None => ""
    }
    connector.get[AnimeSearchResult](s"https://api.jikan.moe/v4/anime?q=$search&page=$page$minScoreExt$maxScoreExt$orderByExt")
  }
}
