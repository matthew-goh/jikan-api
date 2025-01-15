package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import eu.timepit.refined.auto._
import models._
import models.recommendations._
import models.userfavourites._
import models.userprofile.UserProfileResult
import org.scalamock.scalatest.MockFactory
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{JikanService, JikanServiceSpec}

import java.time.OffsetDateTime
import scala.concurrent.{ExecutionContext, Future}

class UserProfileControllerSpec extends BaseSpecWithApplication with MockFactory {
  val mockJikanService: JikanService = mock[JikanService]
  val TestUserProfileController = new UserProfileController(
    mockJikanService,
    component // comes from BaseSpecWithApplication
  )

  def countOccurrences(fullContent: String, target: String): Int =
    fullContent.sliding(target.length).count(window => window == target)
  
  
  "UserProfileController .getUserProfile()" should {
    "display the user's details" in {
      (mockJikanService.getUserProfile(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserProfileResult(JikanServiceSpec.testUserProfile)))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserProfile("Emotional-Yam8")(testRequest.fakeRequest)
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

      val searchResult: Future[Result] = TestUserProfileController.getUserProfile("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }
  }

  "UserProfileController .searchUser()" should {
    "redirect to user details page when a username is searched" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchuser").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8"
      )
      val searchResult: Future[Result] = TestUserProfileController.searchUser()(searchRequest)
      status(searchResult) shouldBe SEE_OTHER
      redirectLocation(searchResult) shouldBe Some("/users/Emotional-Yam8")
    }

    "return a BadRequest if username is blank" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchuser").withFormUrlEncodedBody(
        "username" -> ""
      )
      val searchResult: Future[Result] = TestUserProfileController.searchUser()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("No username provided")
    }
  }

  "UserProfileController .getUserFavouriteAnime()" should {
    "list the user's favourite anime in default order (ascending title)" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites))))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteAnime("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should (include ("Title") and include ("Ascending"))
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent.indexOf("Kindaichi Shounen no Jikenbo") should be < searchResultContent.indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent.indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < searchResultContent.indexOf("Tantei Gakuen Q")
    }

    "list the user's favourite anime sorted by start year in descending order" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites))))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteAnime("Emotional-Yam8", "start_year", "desc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should (include ("Start year") and include ("Descending"))
      searchResultContent should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent.indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < searchResultContent.indexOf("Tantei Gakuen Q")
      searchResultContent.indexOf("Tantei Gakuen Q") should be < searchResultContent.indexOf("Kindaichi Shounen no Jikenbo")
    }

    "show 'No anime favourites' if the user has no favourites listed" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(Seq(), Seq()))))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteAnime("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No anime favourites")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteAnime("abc", "title", "asc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteAnime("Emotional-Yam8", "title", "???")(FakeRequest())
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "UserProfileController .sortFavourites()" should {
    "reload the user's favourite anime page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "orderBy" -> "start_year",
        "sortOrder" -> "asc"
      )
      val sortResult: Future[Result] = TestUserProfileController.sortFavourites("Emotional-Yam8")(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/anime/orderby=start_year/order=asc")
    }

    "set the sort parameters to 'none' if they are missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody()
      val sortResult: Future[Result] = TestUserProfileController.sortFavourites("Emotional-Yam8")(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/anime/orderby=none/order=none")
    }
  }

  "UserProfileController .getUserFavouriteCharacters()" should {
    "list the user's favourite characters" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites))))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteCharacters("Emotional-Yam8")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("<b>2</b> favourite characters on Emotional-Yam8's list")
      searchResultContent should (include ("Isshiki, Totomaru") and include ("Kindaichi, Hajime"))
    }

    "show 'No favourite characters' if the user has no favourites listed" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(Seq(), Seq()))))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteCharacters("Emotional-Yam8")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No favourite characters")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserFavouriteCharacters("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }
  }

  "UserProfileController .getUserRecommendedPairings()" should {
    "list the user's recommended pairings" in {
      (mockJikanService.getUserRecommendations(_: String, _: String)(_: ExecutionContext))
        .expects("veronin", "1", *)
        .returning(EitherT.rightT(JikanServiceSpec.testPairingResult))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserRecommendedPairings("veronin", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Page 1 of 1")
      searchResultContent should include ("06 May 2015")
      searchResultContent should (include ("K-On!!") and include ("Aikatsu!"))

      countOccurrences(searchResultContent, "Posted on:") shouldBe 3
      countOccurrences(searchResultContent, "(Manga)") shouldBe 2 // 1 pairing
    }

    "show 'No recommended pairings' if there are no results" in {
      (mockJikanService.getUserRecommendations(_: String, _: String)(_: ExecutionContext))
        .expects("veronin", "2", *)
        .returning(EitherT.rightT(UserPairingResult(
          SimplePagination(1, has_next_page = false),
          Seq()
        )))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserRecommendedPairings("veronin", "2")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No recommended pairings")
    }

    "return a BadRequest if there is an API result but page number is not a positive integer" in {
      (mockJikanService.getUserRecommendations(_: String, _: String)(_: ExecutionContext))
        .expects("veronin", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testPairingResult))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserRecommendedPairings("veronin", "abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include("API result obtained but page number is not a positive integer")
    }

    "return an InternalServerError if an entry in the API result does not contain a pair" in {
      val badPairing: Pairing = Pairing( // only 1 RecommendationEntry
        Seq(RecommendationEntry(240, "https://myanimelist.net/anime/240/Genshiken", "Genshiken", Images(JpgImage(Some("https://cdn.myanimelist.net/images/anime/1890/94707.jpg?s=1af369e5e0da3322516d1f06f8ecb994"))))),
        "Adult characters with a focus on the anime industry and otaku subculture. They're both pretty unique in a medium that is all very much the same stuff over and over, so do enjoy.",
        OffsetDateTime.parse("2015-05-06T00:00:00+00:00").toInstant
      )

      (mockJikanService.getUserRecommendations(_: String, _: String)(_: ExecutionContext))
        .expects("veronin", "1", *)
        .returning(EitherT.rightT(UserPairingResult(
          SimplePagination(1, has_next_page = false),
          Seq(badPairing)
        )))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserRecommendedPairings("veronin", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(searchResult) should include("Error: An entry returned by the API does not contain a pair")
    }

    "return a NotFound if the username is not found" in {
      (mockJikanService.getUserRecommendations(_: String, _: String)(_: ExecutionContext))
        .expects("abc", "1", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestUserProfileController.getUserRecommendedPairings("abc", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }
}
