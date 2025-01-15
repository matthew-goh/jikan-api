package models.userfavourites

import models._
import play.api.libs.json.{Json, OFormat}

case class AnimeFavourite(mal_id: Int, title: String, `type`: String, start_year: Int, images: Images)

object AnimeFavourite {
  implicit val formats: OFormat[AnimeFavourite] = Json.format[AnimeFavourite]

  implicit class SortableAnimeFavList(val animeList: Seq[AnimeFavourite]) {
    def orderBySortParameter(orderBy: FavouritesOrders.Value, sortOrder: SortOrders.Value): Seq[AnimeFavourite] = {
      val sortedFavList: Seq[AnimeFavourite] = orderBy match {
        case FavouritesOrders.title => animeList.sortBy(_.title)
        case FavouritesOrders.start_year => animeList.sortBy(_.start_year)
        case FavouritesOrders.none => animeList
      }
      if (sortOrder == SortOrders.desc) sortedFavList.reverse
      else sortedFavList
    }
  }
}
