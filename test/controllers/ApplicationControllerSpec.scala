package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import models._
import models.characters._
import models.reviews.ReviewsResult
import org.scalamock.scalatest.MockFactory
import play.api.test.FakeRequest
import play.api.libs.json._
import play.api.mvc._
import play.api.test.Helpers._
import services.{JikanService, JikanServiceSpec}

import java.time.{Instant, OffsetDateTime}
import scala.concurrent.{ExecutionContext, Future}

class ApplicationControllerSpec extends BaseSpecWithApplication with MockFactory {
  val mockJikanService: JikanService = mock[JikanService]
  val TestApplicationController = new ApplicationController(
    repoService,
    mockJikanService,
    component // comes from BaseSpecWithApplication
  )

  override def beforeEach(): Unit = await(repository.deleteAll())
  override def afterEach(): Unit = await(repository.deleteAll())

  private lazy val kindaichi: SavedAnime = SavedAnime(2076, "Kindaichi Shounen no Jikenbo", Some("The File of Young Kindaichi"), "TV", Some(148), Some(1997),
    Some(7.94), Instant.parse("2024-12-18T10:01:49Z"), 148, Some(10), "Best mystery anime")

  private lazy val kubikiri: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 0, None, "")

  private lazy val detectiveSchoolQ: SavedAnime = SavedAnime(407, "Tantei Gakuen Q", Some("Detective School Q"), "TV", Some(45), Some(2003),
    Some(7.73), Instant.parse("2024-12-18T10:01:49Z"), 21, Some(9), "")

  private lazy val kindaichiRefreshed: SavedAnime = SavedAnime(2076, "Kindaichi Shounen no Jikenbo", Some("The File of Young Kindaichi"), "TV", Some(148), Some(1997),
    Some(7.97), Instant.parse("2024-12-18T10:01:49Z"), 148, Some(10), "Best mystery anime")

  private lazy val kubikiriUpdated: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 4, None, "Closed circle mystery on an island")

  private lazy val kubikiriData: AnimeData = AnimeData(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), "Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2016-10-26T00:00:00+00:00").toInstant), Some(OffsetDateTime.parse("2017-09-27T00:00:00+00:00").toInstant)), Some("R - 17+ (violence & profanity)"), Some(7.75), Some(34440),
    Some("""Due to a mysterious disease, the genius Iria Akagami has been forced by her family to stay in a mansion on the isolated Wet Crow's Feather Island with only a handful of maids. To keep herself entertained, Iria invites a variety of fellow geniuses to stay as guests in her home, including computer savant Tomo Kunagisa and her unnamed assistant, skilled fortune-teller Maki Himena, famous artist Kanami Ibuki, academic scholar Akane Sonoyama, and renowned cook Yayoi Sashirono.
           |
           |These visits progress as normal until one of the guests is found gruesomely murdered in the night without a single clue as to the identity of the killer or a possible motive. Tensions rise between those on the island as the killer remains at large, and Tomo's assistant takes it upon himself to uncover the culprit's identity before the murderous events progress any further.
           |
           |[Written by MAL Rewrite]""".stripMargin),
    List(Genre(8, "Drama"), Genre(7, "Mystery"), Genre(37, "Supernatural")), None)

  def countOccurrences(fullContent: String, target: String): Int =
    fullContent.sliding(target.length).count(window => window == target)

  ///// METHODS FOCUSING ON CONNECTOR /////
  /// 1. Anime search ///
  "ApplicationController .getAnimeResults()" should {
    "list the anime search results" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

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
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should (include ("Page 1 of 1") and include ("Results 1-9"))
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should include ("Average score: 7.94")
      searchResultContent should include ("Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken")
      searchResultContent should include ("from 606 users")

      countOccurrences(searchResultContent, "Saved") shouldBe 1
      countOccurrences(searchResultContent, "+ Save") shouldBe 8
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
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should (include ("Complete") and include ("No. of episodes") and include ("Descending"))
      searchResultContent should include ("Results 1-3")
      searchResultContent should include ("The File of Young Kindaichi")
      searchResultContent.indexOf("The File of Young Kindaichi") should be < searchResultContent.indexOf("Average score: 7.54")
      searchResultContent.indexOf("Average score: 7.54") should be < searchResultContent.indexOf("Kindaichi Shounen no Jikenbo Returns 2nd Season")
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
      redirectLocation(searchResult) shouldBe Some("/searchanime/kindaichi/page=1/status=complete&min_score=&max_score=8.5&order_by=title&sort=")
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
    "display the anime's details when it is not saved in the database" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo (MAL ID: 2076)")
      contentAsString(searchResult) should include ("Mon, 11 Sep 2000")
      contentAsString(searchResult) should include ("Scored by 8,317 users")
      contentAsString(searchResult) should include ("+ Save")
    }

    "display the anime's details when it is already saved in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED
      
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo (MAL ID: 2076)")
      contentAsString(searchResult) should include ("Saved")
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

  /// 2. User profiles ///
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

  "ApplicationController .getUserFavouriteAnime()" should {
    "list the user's favourite anime in default order (ascending title)" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteAnime("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
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

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteAnime("Emotional-Yam8", "start_year", "desc")(testRequest.fakeRequest)
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

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteAnime("Emotional-Yam8", "title", "asc")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No anime favourites")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteAnime("abc", "title", "asc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteAnime("Emotional-Yam8", "title", "???")(FakeRequest())
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "ApplicationController .sortFavourites()" should {
    "reload the user's favourite anime page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8",
        "orderBy" -> "start_year",
        "sortOrder" -> "asc"
      )
      val sortResult: Future[Result] = TestApplicationController.sortFavourites()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/anime/orderby=start_year/order=asc")
    }

    "set the sort parameters to 'none' if they are missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortfavourites").withFormUrlEncodedBody(
        "username" -> "Emotional-Yam8"
      )
      val sortResult: Future[Result] = TestApplicationController.sortFavourites()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/users/Emotional-Yam8/favourites/anime/orderby=none/order=none")
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

  "ApplicationController .getUserFavouriteCharacters()" should {
    "list the user's favourite characters" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteCharacters("Emotional-Yam8")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("<b>2</b> favourite characters on Emotional-Yam8's list.")
      searchResultContent should (include ("Isshiki, Totomaru") and include ("Kindaichi, Hajime"))
    }

    "show 'No favourite characters' if the user has no favourites listed" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("Emotional-Yam8", *)
        .returning(EitherT.rightT(UserFavouritesResult(UserFavouritesData(Seq(), Seq()))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteCharacters("Emotional-Yam8")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No favourite characters")
    }

    "return a NotFound if the user is not found" in {
      (mockJikanService.getUserFavourites(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getUserFavouriteCharacters("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }
  }

  /// 3. Anime extra info ///
  "ApplicationController .getEpisodeList()" should {
    "list the anime's episodes" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeEpisodes(_: String, _: String)(_: ExecutionContext))
        .expects("33263", "1", *)
        .returning(EitherT.rightT(JikanServiceSpec.testEpisodeSearchResult))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("33263", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent should (include ("Page 1 of 1") and include ("Episodes 1-8 of 8"))
      searchResultContent should include ("Day 3 (1) The Savant Gathering")
      searchResultContent should include ("Wed, 26 Oct 2016")
      searchResultContent should include ("Episode 8")
      searchResultContent should include ("Wed, 27 Sep 2017")

      countOccurrences(searchResultContent, "Aired") shouldBe 8
    }

    "show 'No episode data available' if there are no results" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeEpisodes(_: String, _: String)(_: ExecutionContext))
        .expects("33263", "2", *)
        .returning(EitherT.rightT(EpisodeSearchResult(
          SimplePagination(1, has_next_page = false),
          Seq()
        )))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("33263", "2")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No episode data available")
    }

    "return a BadRequest if there is an API result but page number is not an integer" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeEpisodes(_: String, _: String)(_: ExecutionContext))
        .expects("33263", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testEpisodeSearchResult))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("33263", "abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("API result obtained but page number is not an integer")
    }

    "return a BadRequest if the API detects an invalid page number" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeEpisodes(_: String, _: String)(_: ExecutionContext))
        .expects("33263", "0", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The page must be at least 1.")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("33263", "0")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Bad response from upstream: The page must be at least 1.")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("abc", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getSingleEpisodeDetails()" should {
    "display the episode's details" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeEpisodeDetails(_: String, _: String)(_: ExecutionContext))
        .expects("2076", "143", *)
        .returning(EitherT.rightT(SingleEpisodeResult(JikanServiceSpec.testEpisodeDetails)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getSingleEpisodeDetails("2076", "143")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should include ("Episode 143")
      searchResultContent should include ("This is neither a filler nor a recap episode.")
      searchResultContent should include ("24:00")
      searchResultContent should include ("Mon, 07 Aug 2000")
    }

    "show 'No episode data available' if the result contains no data (e.g. if episode does not exist)" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeEpisodeDetails(_: String, _: String)(_: ExecutionContext))
        .expects("2076", "150", *)
        .returning(EitherT.rightT(SingleEpisodeResult(
          EpisodeFullDetails(150, "", None, None, filler = false, recap = false, None))
        ))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getSingleEpisodeDetails("2076", "150")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No episode data available")
      contentAsString(searchResult) should include ("This anime only has 148 episodes")
    }

    "return a BadRequest if the episode ID is invalid" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeEpisodeDetails(_: String, _: String)(_: ExecutionContext))
        .expects("2076", "0", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The episode id must be at least 1.")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getSingleEpisodeDetails("2076", "0")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Bad response from upstream: The episode id must be at least 1.")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getEpisodeList("abc", "1")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getAnimeCharacters()" should {
    "list the anime's characters" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeCharacters(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(CharacterSearchResult(JikanServiceSpec.testCharacters)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeCharacters("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent should (include ("Boku") and include ("Kunagisa, Tomo") and include ("Aikawa, Jun") and include ("Akagami, Iria"))
      searchResultContent should include ("Favourited by 1 user")

      countOccurrences(searchResultContent, "Favourited by") shouldBe 4
    }

    "show 'No characters to display' if there are no results" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeCharacters(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(CharacterSearchResult(Seq())))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeCharacters("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No characters to display")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeCharacters("abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getCharacterProfile()" should {
    "display a character's profile" in {
      (mockJikanService.getCharacterProfile(_: String)(_: ExecutionContext))
        .expects("192285", *)
        .returning(EitherT.rightT(CharacterProfileResult(JikanServiceSpec.testCharacterProfile)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getCharacterProfile("192285")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Totomaru Isshiki")
      searchResultContent should include ("<b>Nicknames:</b> Toto")
      searchResultContent should include ("Kamonohashi Ron no Kindan Suiri 2nd Season")
      countOccurrences(searchResultContent, "Main role") shouldBe 2
    }

    "display a character's profile with no biography or anime appearances" in {
      val characterInNoAnime: CharacterProfile = CharacterProfile(192285,
        CharacterImages(JpgImage("https://cdn.myanimelist.net/images/characters/11/516963.jpg")),
        "Character Name", Seq("Nickname 1", "Nickname 2"), 0, None, Seq())

      (mockJikanService.getCharacterProfile(_: String)(_: ExecutionContext))
        .expects("192285", *)
        .returning(EitherT.rightT(CharacterProfileResult(characterInNoAnime)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getCharacterProfile("192285")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("<b>Nicknames:</b> Nickname 1, Nickname 2")
      searchResultContent should include ("<b>About:</b><br>Not available")
      searchResultContent should include ("Not appearing in any anime.")
    }

    "return a NotFound if the character is not found" in {
      (mockJikanService.getCharacterProfile(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getCharacterProfile("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getAnimeReviews()" should {
    "display anime reviews, including preliminary and spoiler reviews" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeReviews(_: String, _: String, _: String, _: String)(_: ExecutionContext))
        .expects("2076", "1", "true", "true", *)
        .returning(EitherT.rightT(JikanServiceSpec.testReviewsResult))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("2076", "1", "true", "true")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should (include ("MasterGhost") and include ("04 Mar 2019 03:21") and include("Recommended, Preliminary, Spoiler"))

      countOccurrences(searchResultContent, "This review is preliminary.") shouldBe 1
      countOccurrences(searchResultContent, "Warning! This review contains spoilers.") shouldBe 2
      countOccurrences(searchResultContent, "checked") shouldBe 2  // checkboxes
    }

    "display anime reviews, including spoiler reviews but not preliminary reviews" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeReviews(_: String, _: String, _: String, _: String)(_: ExecutionContext))
        .expects("2076", "1", "false", "true", *)
        .returning(EitherT.rightT(ReviewsResult(Seq(JikanServiceSpec.testReview1, JikanServiceSpec.testReview2), SimplePagination(1, has_next_page = false))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("2076", "1", "false", "true")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should (include ("MasterGhost") and include("Recommended, Spoiler"))
      searchResultContent shouldNot include ("This review is preliminary.")

      countOccurrences(searchResultContent, "Warning! This review contains spoilers.") shouldBe 1
      countOccurrences(searchResultContent, "checked") shouldBe 1  // checkboxes
    }

    "show 'No reviews available' if there are no reviews" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeReviews(_: String, _: String, _: String, _: String)(_: ExecutionContext))
        .expects("2076", "1", "true", "true", *)
        .returning(EitherT.rightT(ReviewsResult(Seq(), SimplePagination(1, has_next_page = false))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("2076", "1", "true", "true")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No reviews available")
    }

    "return a BadRequest if the API returned a result but a query parameter is invalid" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeReviews(_: String, _: String, _: String, _: String)(_: ExecutionContext))
        .expects("2076", "1", "ff", "ff", *)
        .returning(EitherT.rightT(JikanServiceSpec.testReviewsResult))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("2076", "1", "ff", "ff")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("API result obtained but a query parameter is invalid")
    }

    "return a BadRequest if the API detects an invalid query parameter" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeReviews(_: String, _: String, _: String, _: String)(_: ExecutionContext))
        .expects("2076", "1", "false", "f", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The spoilers field must be true or false.")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("2076", "1", "false", "f")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Bad response from upstream: The spoilers field must be true or false.")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeReviews("abc", "1", "false", "false")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .filterReviews()" should {
    "reload the anime reviews page with the correct parameters (where missing means false)" in {
      val filterRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/filterreviews/2076").withFormUrlEncodedBody(
        "preliminary" -> "true"
      )
      val filterResult: Future[Result] = TestApplicationController.filterReviews("2076")(filterRequest)
      status(filterResult) shouldBe SEE_OTHER
      redirectLocation(filterResult) shouldBe Some("/anime/2076/reviews/page=1/preliminary=true/spoilers=false")
    }

    "return a BadRequest if an invalid parameter value was submitted" in {
      val filterRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/filterreviews/2076").withFormUrlEncodedBody(
        "preliminary" -> "true",
        "spoilers" -> "ff"
      )
      val filterResult: Future[Result] = TestApplicationController.filterReviews("2076")(filterRequest)
      status(filterResult) shouldBe BAD_REQUEST
      contentAsString(filterResult) should include ("Invalid value submitted for spoilers")
    }
  }


  ///// METHODS FOCUSING ON REPOSITORY /////
  "ApplicationController .listSavedAnime()" should {
    "list all saved anime in default order (ascending saved_at)" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Future[Result] = TestApplicationController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult3: Future[Result] = TestApplicationController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestApplicationController.listSavedAnime("all", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo") should be < contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
      countOccurrences(contentAsString(listingResult), "Unsave") shouldBe 3
    }

    "list all saved anime sorted by the user's score in descending order" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Future[Result] = TestApplicationController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult3: Future[Result] = TestApplicationController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestApplicationController.listSavedAnime("all", "score", "desc")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
      contentAsString(listingResult).indexOf("Tantei Gakuen Q") should be < contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
    }

    "show 'No saved anime' if the database is empty" in {
      val listingResult: Future[Result] = TestApplicationController.listSavedAnime("all", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime.")
    }

    "show 'No saved anime' if no saved anime matches the selected completion status" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val listingResult: Future[Result] = TestApplicationController.listSavedAnime("not_started", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime.")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestApplicationController.listSavedAnime("all", "?", "?")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "ApplicationController .sortSavedList()" should {
    "reload the saved anime page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortsaved").withFormUrlEncodedBody(
        "completionStatus" -> "completed",
        "orderBy" -> "score",
        "sortOrder" -> "asc"
      )
      val sortResult: Future[Result] = TestApplicationController.sortSavedList()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/saved/status=completed/orderby=score/order=asc")
    }

    "set the sort parameters to default values if they are missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortsaved").withFormUrlEncodedBody(
        "sortOrder" -> "none"
      )
      val sortResult: Future[Result] = TestApplicationController.sortSavedList()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/saved/status=all/orderby=saved_at/order=none")
    }
  }

  "ApplicationController .listSavedAnimeFromTitleSearch()" should {
    "list saved anime whose title contains the searched title (in alphabetical order)" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult2: Future[Result] = TestApplicationController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult3: Future[Result] = TestApplicationController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestApplicationController.listSavedAnimeFromTitleSearch("ku")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("2 matching saved anime")
      contentAsString(listingResult) should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
    }

    "show 'No saved anime' if no saved anime matches the searched title" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val listingResult: Future[Result] = TestApplicationController.listSavedAnimeFromTitleSearch("kubikiri")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime matching the searched title.")
    }
  }

  "ApplicationController .searchSavedAnimeByTitle()" should {
    "redirect to saved anime title search page when a title is searched" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saved/titlesearch").withFormUrlEncodedBody(
        "title" -> "kindaichi"
      )
      val searchResult: Future[Result] = TestApplicationController.searchSavedAnimeByTitle()(searchRequest)
      status(searchResult) shouldBe SEE_OTHER
      redirectLocation(searchResult) shouldBe Some("/saved/titlesearch/kindaichi")
    }

    "return a BadRequest if searched title is blank" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saved/titlesearch").withFormUrlEncodedBody(
        "title" -> ""
      )
      val searchResult: Future[Result] = TestApplicationController.searchSavedAnimeByTitle()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("No search title submitted")
    }
  }

  "ApplicationController .viewSavedAnime()" should {
    "display the saved anime's details" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val viewResult: Future[Result] = TestApplicationController.viewSavedAnime("33263")(testRequest.fakeRequest)
      status(viewResult) shouldBe OK
      contentAsString(viewResult) should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(viewResult) should include ("<b>Saved at:</b> 18 Dec 2024 10:01")
      contentAsString(viewResult) should include ("Not scored")
    }

    "return a NotFound if the anime is not saved in the database" in {
      val viewResult: Future[Result] = TestApplicationController.viewSavedAnime("2076")(testRequest.fakeRequest)
      status(viewResult) shouldBe NOT_FOUND
      contentAsString(viewResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val viewResult: Future[Result] = TestApplicationController.viewSavedAnime("abc")(testRequest.fakeRequest)
      status(viewResult) shouldBe BAD_REQUEST
      contentAsString(viewResult) should include ("Anime ID must be an integer")
    }
  }

  "ApplicationController .saveAnime()" should {
    "save an anime to the database" in {
      val saveRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "url" -> "/anime/33263",
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75"
      )
      val saveResult: Future[Result] = TestApplicationController.saveAnime()(saveRequest)
      status(saveResult) shouldBe OK
      contentAsString(saveResult) should include ("Anime saved!")
    }

    "return an InternalServerError if the anime is already in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val saveRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "url" -> "/anime/33263",
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75"
      )
      val saveResult: Future[Result] = TestApplicationController.saveAnime()(saveRequest)
      status(saveResult) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(saveResult) should include ("Bad response from upstream: Anime has already been saved")
    }

    "return a BadRequest if posted source URL is missing" in {
      val saveRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "id" -> "33263"
      )
      val saveResult: Future[Result] = TestApplicationController.saveAnime()(saveRequest)
      status(saveResult) shouldBe BAD_REQUEST
      contentAsString(saveResult) should include ("Failed to post source url")
    }
  }

  "ApplicationController .refreshSavedAnime()" should {
    "refresh an anime's MAL details in the database" in {
      val kindaichiDataRefreshed: AnimeData = AnimeData(2076,"Kindaichi Shounen no Jikenbo",Some("The File of Young Kindaichi"),"TV",Some(148),"Finished Airing",
        AirDates(Some(OffsetDateTime.parse("1997-04-07T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2000-09-11T00:00:00+00:00").toInstant)),
        Some("R - 17+ (violence & profanity)"),Some(7.97),Some(8317),
        Some("""
         |Hajime Kindaichi's unorganized appearance and lax nature may give the impression of an average high school student, but a book should never be judged by its cover. Hajime is the grandson of the man who was once Japan's greatest detective, and he is also a remarkable sleuth himself.
         |
         |With the help of his best friend, Miyuki Nanase, and the peculiar inspector Isamu Kenmochi, Hajime travels to remote islands, ominous towns, abysmal seas, and other hostile environments. His life's mission is to uncover the truth behind some of the most cunning, grueling, and disturbing mysteries the world has ever faced.
         |
         |[Written by MAL Rewrite]""".stripMargin),List(Genre(7,"Mystery")),Some(1997))

      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh").withFormUrlEncodedBody(
        "url" -> "/saved/2076",
        "id" -> "2076",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "148",
        "score" -> "10",
        "notes" -> "Best mystery anime"
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kindaichiDataRefreshed)))
        .once()

      val refreshResult: Future[Result] = TestApplicationController.refreshSavedAnime()(refreshRequest)
      status(refreshResult) shouldBe OK
      contentAsString(refreshResult) should include ("Anime details refreshed!")

      val indexResult: Future[Result] = TestApplicationController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kindaichiRefreshed)
    }

    "return a NotFound if the anime ID cannot be found in the database" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh").withFormUrlEncodedBody(
        "url" -> "/saved/2076",
        "id" -> "2076",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "148",
        "score" -> "10",
        "notes" -> "Best mystery anime"
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      val refreshResult: Future[Result] = TestApplicationController.refreshSavedAnime()(refreshRequest)
      status(refreshResult) shouldBe NOT_FOUND
      contentAsString(refreshResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a NotFound if the anime ID does not exist on MAL" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh").withFormUrlEncodedBody(
        "url" -> "/saved/2076",
        "id" -> "2076",
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val refreshResult: Future[Result] = TestApplicationController.refreshSavedAnime()(refreshRequest)
      status(refreshResult) shouldBe NOT_FOUND
      contentAsString(refreshResult) should include ("Bad response from upstream: Not Found")
    }

    "return a BadRequest if posted anime ID is invalid" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh").withFormUrlEncodedBody(
        "url" -> "/anime/2076",
        "id" -> "abc"
      )
      val refreshResult: Future[Result] = TestApplicationController.refreshSavedAnime()(refreshRequest)
      status(refreshResult) shouldBe BAD_REQUEST
      contentAsString(refreshResult) should include ("Invalid or missing anime ID")
    }

    "return a BadRequest if posted source URL is missing" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh").withFormUrlEncodedBody(
        "id" -> "2076"
      )
      val refreshResult: Future[Result] = TestApplicationController.refreshSavedAnime()(refreshRequest)
      status(refreshResult) shouldBe BAD_REQUEST
      contentAsString(refreshResult) should include ("Failed to post source url")
    }
  }

  "ApplicationController .showUpdateForm()" should {
    "display the update form with existing data filled in" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val formResult: Future[Result] = TestApplicationController.showUpdateForm("2076")(testRequest.fakeRequest)
      status(formResult) shouldBe OK
      contentAsString(formResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(formResult) should include ("No. of episodes: 148")
      contentAsString(formResult) should include ("Best mystery anime")
    }

    "return a NotFound if the anime is not saved in the database" in {
      val formResult: Future[Result] = TestApplicationController.showUpdateForm("2076")(testRequest.fakeRequest)
      status(formResult) shouldBe NOT_FOUND
      contentAsString(formResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val formResult: Future[Result] = TestApplicationController.showUpdateForm("abc")(testRequest.fakeRequest)
      status(formResult) shouldBe BAD_REQUEST
      contentAsString(formResult) should include ("Anime ID must be an integer")
    }
  }

  "ApplicationController .updateFormSubmit()" should {
    "update anime details in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val updateRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/update/33263").withFormUrlEncodedBody(
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "4",
        "score" -> "",
        "notes" -> "Closed circle mystery on an island"
      )
      val updateResult: Future[Result] = TestApplicationController.updateFormSubmit("33263")(updateRequest)
      status(updateResult) shouldBe OK
      contentAsString(updateResult) should include ("Anime details updated!")

      val indexResult: Future[Result] = TestApplicationController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kubikiriUpdated)
    }

    "return a NotFound if the anime is not in the database" in {
      val updateRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/update/33263").withFormUrlEncodedBody(
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "4",
        "score" -> "",
        "notes" -> "Closed circle mystery on an island"
      )
      val updateResult: Future[Result] = TestApplicationController.updateFormSubmit("33263")(updateRequest)
      status(updateResult) shouldBe NOT_FOUND
      contentAsString(updateResult) should include ("Bad response from upstream: Anime not saved in database")
    }
  }

  "ApplicationController .unsaveAnime()" should {
    "delete an anime from the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val deleteResult: Future[Result] = TestApplicationController.unsaveAnime("2076")(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("Anime removed from saved list")
    }

    "return a NotFound if the anime is not in the database" in {
      val deleteResult: Future[Result] = TestApplicationController.unsaveAnime("2076")(FakeRequest())
      status(deleteResult) shouldBe NOT_FOUND
      contentAsString(deleteResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val viewResult: Future[Result] = TestApplicationController.unsaveAnime("abc")(FakeRequest())
      status(viewResult) shouldBe BAD_REQUEST
      contentAsString(viewResult) should include ("Anime ID must be an integer")
    }
  }

  "ApplicationController .deleteAll() (test-only method)" should {
    "delete all anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Result = await(TestApplicationController.create()(request))
      createdResult.header.status shouldBe CREATED

      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Result = await(TestApplicationController.create()(request2))
      createdResult2.header.status shouldBe CREATED

      val deleteResult: Future[Result] = TestApplicationController.deleteAll()(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("All saved anime removed from database.")

      // check that database is now empty
      val indexResult: Future[Result] = TestApplicationController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq()
    }

    "return the correct message if there are no anime saved in the database" in {
      val deleteResult: Future[Result] = TestApplicationController.deleteAll()(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("No saved anime to delete.")
    }
  }

  ///// NON-FRONTEND METHODS, FOR TESTING /////
  "ApplicationController .index()" should {
    "list all anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Result = await(TestApplicationController.create()(request))
      createdResult.header.status shouldBe CREATED

      val indexResult: Future[Result] = TestApplicationController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kindaichi)
    }
  }

  "ApplicationController .create()" should {
    "save an anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED
      contentAsJson(createdResult).as[SavedAnime] shouldBe kindaichi
    }

    "return an InternalServerError if the anime is already in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe CREATED

      val duplicateRequest: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val duplicateResult: Future[Result] = TestApplicationController.create()(duplicateRequest)
      status(duplicateResult) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(duplicateResult) shouldBe "Bad response from upstream: Anime has already been saved"
    }

    "return a BadRequest if the request body could not be parsed into a SavedAnime" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson("abcd"))
      val createdResult: Future[Result] = TestApplicationController.create()(request)
      status(createdResult) shouldBe BAD_REQUEST
      contentAsString(createdResult) shouldBe "Invalid request body"
    }
  }
}
