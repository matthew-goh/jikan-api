package models.news

import be.venneborg.refined.play.RefinedJsonFormats._
import models.Images
import models.RefinedTypes.UrlString
import play.api.libs.json.{Json, OFormat}

import java.time.Instant

case class AnimeNews(title: String, url: UrlString, date: Instant, author_username: String, images: Images, excerpt: String)

object AnimeNews {
  implicit val formats: OFormat[AnimeNews] = Json.format[AnimeNews]
}
