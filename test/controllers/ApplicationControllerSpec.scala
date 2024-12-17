package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import models._
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import org.scalatest.concurrent.ScalaFutures
import play.api.test.FakeRequest
import play.api.libs.json._
import play.api.mvc._
import play.api.test.Helpers._
import services.{JikanService, JikanServiceSpec}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class ApplicationControllerSpec extends BaseSpecWithApplication with MockFactory {
  val mockJikanService: JikanService = mock[JikanService]
  val TestApplicationController = new ApplicationController(
//    repoService,
    mockJikanService,
    component // comes from BaseSpecWithApplication
  )

  "ApplicationController .getAnimeResults()" should {
    "list the anime search results" in {
      (mockJikanService.getAnimeSearchResults(_: String, _: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects("kindaichi", "1", "status=&min_score=&max_score=&order_by=&sort=", None, *)
        .returning(EitherT.rightT(JikanServiceSpec.testAnimeSearchResult))
        .once()

      (mockJikanService.queryExtToAnimeSearchParams(_: String))
        .expects("status=&min_score=&max_score=&order_by=&sort=")
        .returning(AnimeSearchParams("","","","",""))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeResults("kindaichi", "1", "status=&min_score=&max_score=&order_by=&sort=")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should (include ("Page 1 of 1") and include ("Results 1-9"))
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(searchResult) should include ("Average score: 7.94")
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken")
      contentAsString(searchResult) should include ("from 606 users")
    }

    "list the anime search results and retain the parameter values under 'Modify Search'" in {
      (mockJikanService.getAnimeSearchResults(_: String, _: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects("kindaichi", "1", "status=complete&min_score=7.5&max_score=8&order_by=episodes&sort=desc", None, *)
        .returning(EitherT.rightT(
          AnimeSearchResult(AnimeSearchPagination(1, 1, has_next_page = false, AnimeSearchPagItems(3, 3, 25)),
            Seq(JikanServiceSpec.kindaichiData1, JikanServiceSpec.kindaichiData2, JikanServiceSpec.kindaichiData4))
        ))
        .once()

      (mockJikanService.queryExtToAnimeSearchParams(_: String))
        .expects("status=complete&min_score=7.5&max_score=8&order_by=episodes&sort=desc")
        .returning(AnimeSearchParams("complete","7.5","8","episodes","desc"))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeResults("kindaichi", "1", "status=complete&min_score=7.5&max_score=8&order_by=episodes&sort=desc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Results 1-3")
//      contentAsString(searchResult) should include regex "The File of Young Kindaichi.*Kindaichi Shounen no Jikenbo Returns 2nd Season.*Kindaichi Shounen no Jikenbo Returns".r
      contentAsString(searchResult) should include ("The File of Young Kindaichi")
      contentAsString(searchResult).indexOf("The File of Young Kindaichi") should be < contentAsString(searchResult).indexOf("Average score: 7.54")
      contentAsString(searchResult).indexOf("Average score: 7.54") should be < contentAsString(searchResult).indexOf("Kindaichi Shounen no Jikenbo Returns 2nd Season")

      contentAsString(searchResult) should (include ("Complete") and include ("No. of episodes") and include ("Descending"))
    }

    "show 'No search results' if there are no results" in {
      (mockJikanService.getAnimeSearchResults(_: String, _: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects("gesthbghdrthb", "1", "status=&min_score=&max_score=&order_by=&sort=", None, *)
        .returning(EitherT.rightT(AnimeSearchResult(JikanServiceSpec.testSearchPagination, Seq())))
        .once()

      (mockJikanService.queryExtToAnimeSearchParams(_: String))
        .expects("status=&min_score=&max_score=&order_by=&sort=")
        .returning(AnimeSearchParams("","","","",""))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeResults("gesthbghdrthb", "1", "status=&min_score=&max_score=&order_by=&sort=")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No search results")
    }

    "return a BadRequest if queryExt contains invalid parameter values" in {
      (mockJikanService.getAnimeSearchResults(_: String, _: String, _: String, _: Option[String])(_: ExecutionContext))
        .expects("kindaichi", "1", "status=&min_score=&max_score=22&order_by=&sort=", None, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The max score must be between 1 and 10.")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeResults("kindaichi", "1", "status=&min_score=&max_score=22&order_by=&sort=")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Bad response from upstream: The max score must be between 1 and 10.")
    }
  }

  "ApplicationController .searchAnime()" should {
    "redirect to search results page when a valid search is performed" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchanime").withFormUrlEncodedBody(
        "search" -> "kindaichi",
        "status" -> "complete",
        "minScore" -> "",
        "maxScore" -> "8.5",
        "orderBy" -> "title",
        "sort" -> ""
      )
      val searchResult: Future[Result] = TestApplicationController.searchAnime()(searchRequest)
      status(searchResult) shouldBe SEE_OTHER
      redirectLocation(searchResult) shouldBe Some("/searchanime/kindaichi/1/status=complete&min_score=&max_score=8.5&order_by=title&sort=")
    }

    "return a BadRequest if search term is blank" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchanime").withFormUrlEncodedBody(
        "search" -> "",
        "status" -> "",
        "minScore" -> "",
        "maxScore" -> "8.5",
        "orderBy" -> "title",
        "sort" -> ""
      )
      val searchResult: Future[Result] = TestApplicationController.searchAnime()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("No search term provided")
    }
  }

  "ApplicationController .getAnimeById()" should {
    "display the anime's details" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo (MAL ID: 2076)")
      contentAsString(searchResult) should include ("Mon, 11 Sep 2000")
      contentAsString(searchResult) should include ("Scored by 8,317 users")
    }

    "return a NotFound if the anime is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getUserProfile()" should {
    "display the user's details" in {
      (mockJikanService.getUserProfile(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserProfileResult(JikanServiceSpec.testUserProfile)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserProfile("Emotional-Yam8")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("<b>Username</b>: Emotional-Yam8")
      contentAsString(searchResult) should include ("<b>Location</b>: Not provided")
      contentAsString(searchResult) should include ("16 Dec 2024 17:00")
      contentAsString(searchResult) should include ("<b>91</b> anime completed")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserProfile(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserProfile("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }
  }

  "ApplicationController .searchUser()" should {
    "redirect to user details page when a username is searched" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchuser").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8"
      )
      val searchResult: Future[Result] = TestApplicationController.searchUser()(searchRequest)
      status(searchResult) shouldBe SEE_OTHER
      redirectLocation(searchResult) shouldBe Some("/users/Emotional-Yam8")
    }

    "return a BadRequest if username is blank" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchuser").withFormUrlEncodedBody(
        "username" -> ""
      )
      val searchResult: Future[Result] = TestApplicationController.searchUser()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("No username provided")
    }
  }

  "ApplicationController .getUserFavourites()" should {
    "list the user's favourites in default order (ascending title)" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testFavouritesList))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavourites("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should (include ("Title") and include ("Ascending"))
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(searchResult).indexOf("Kindaichi Shounen no Jikenbo") should be < contentAsString(searchResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(searchResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(searchResult).indexOf("Tantei Gakuen Q")
    }

    "list the user's favourites sorted by start year in descending order" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testFavouritesList))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavourites("Emotional-Yam8", "start_year", "desc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should (include ("Start year") and include ("Descending"))
      contentAsString(searchResult) should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(searchResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(searchResult).indexOf("Tantei Gakuen Q")
      contentAsString(searchResult).indexOf("Tantei Gakuen Q") should be < contentAsString(searchResult).indexOf("Kindaichi Shounen no Jikenbo")
    }

    "show 'No favourites' if the user has no favourites" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(Seq()))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavourites("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No favourites")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavourites("abc", "title", "asc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestApplicationController.getUserFavourites("Emotional-Yam8", "title", "???")(FakeRequest())
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "ApplicationController .sortFavourites()" should {
    "reload the user favourites page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8",
        "orderBy" -> "start_year",
        "sortOrder" -> "asc"
      )
      val sortResult: Future[Result] = TestApplicationController.sortFavourites()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/start_year/asc")
    }

    "set the sort parameters to 'none' if they are somehow missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8"
      )
      val sortResult: Future[Result] = TestApplicationController.sortFavourites()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/none/none")
    }

    "return a BadRequest if username (hidden field) is blank" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "username" -> "",
        "orderBy" -> "start_year",
        "sortOrder" -> "none"
      )
      val sortResult: Future[Result] = TestApplicationController.sortFavourites()(sortRequest)
      status(sortResult) shouldBe BAD_REQUEST
      contentAsString(sortResult) should include ("No username submitted")
    }
  }
}
