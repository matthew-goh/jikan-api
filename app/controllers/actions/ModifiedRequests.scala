package controllers.actions

import models.SavedAnime
import play.api.mvc.{Request, WrappedRequest}

object ModifiedRequests {
  case class RequestWithUrl[A](sourceUrl: String, request: Request[A]) extends WrappedRequest[A](request)
  case class SaveAnimeRequest[A](animeToSave: SavedAnime, sourceUrl: String, request: Request[A]) extends WrappedRequest[A](request)
}
