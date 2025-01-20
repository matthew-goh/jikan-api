package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import eu.timepit.refined.auto._
import models._
import models.characters._
import models.episodes._
import models.news.NewsResult
import models.people.StaffResult
import models.recommendations._
import models.relations._
import models.reviews.ReviewsResult
import models.statistics._
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
  val TestSavedAnimeController = new SavedAnimeController(
    repoService,
    mockJikanService,
    urlActionBuilder,
    saveAnimeRefiner,
    component // comes from BaseSpecWithApplication
  )

  override def beforeEach(): Unit = await(repository.deleteAll())
  override def afterEach(): Unit = await(repository.deleteAll())

  private lazy val kindaichi: SavedAnime = SavedAnime(2076, "Kindaichi Shounen no Jikenbo", Some("The File of Young Kindaichi"), "TV", Some(148), Some(1997),
    Some(7.94), Instant.parse("2024-12-18T10:01:49Z"), 148, Some(10), "Best mystery anime")

  private lazy val testImage: Images = Images(JpgImage(Some("https://cdn.myanimelist.net/images/anime/12/81588.jpg")))
  private lazy val kubikiriData: AnimeData = AnimeData(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), "Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2016-10-26T00:00:00+00:00").toInstant), Some(OffsetDateTime.parse("2017-09-27T00:00:00+00:00").toInstant)), Some("R - 17+ (violence & profanity)"), Some(7.75), Some(34440),
    Some("""Due to a mysterious disease, the genius Iria Akagami has been forced by her family to stay in a mansion on the isolated Wet Crow's Feather Island with only a handful of maids. To keep herself entertained, Iria invites a variety of fellow geniuses to stay as guests in her home, including computer savant Tomo Kunagisa and her unnamed assistant, skilled fortune-teller Maki Himena, famous artist Kanami Ibuki, academic scholar Akane Sonoyama, and renowned cook Yayoi Sashirono.
           |
           |These visits progress as normal until one of the guests is found gruesomely murdered in the night without a single clue as to the identity of the killer or a possible motive. Tensions rise between those on the island as the killer remains at large, and Tomo's assistant takes it upon himself to uncover the culprit's identity before the murderous events progress any further.
           |
           |[Written by MAL Rewrite]""".stripMargin),
    List(Genre(8, "Drama"), Genre(7, "Mystery"), Genre(37, "Supernatural")), None, testImage)

  def countOccurrences(fullContent: String, target: String): Int =
    fullContent.sliding(target.length).count(window => window == target)

  /// Anime search ///
  "ApplicationController .getAnimeResults()" should {
    "list the anime search results" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
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
      searchResultContent should (include ("Page 1 of 1") and include ("Results 1 - 9"))
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should include ("from 8,317 users")
      searchResultContent should include ("Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken")
      searchResultContent should include ("Special, 1997") // used year from AirDates

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
      searchResultContent should include ("Results 1 - 3")
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

    "return a BadRequest if expected search parameters are missing" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/searchanime").withFormUrlEncodedBody(
        "search" -> "kindaichi",
        "status" -> "",
        "minScore" -> "",
        "maxScore" -> "8.5",
        "orderBy" -> "title",
        "srt" -> "" // wrong
      )
      val searchResult: Future[Result] = TestApplicationController.searchAnime()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid search parameters submitted")
    }
  }

  "ApplicationController .getAnimeById()" should {
    "display the anime's details when it is not saved in the database and has multiple images" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getImageList(_: String, _: ImageListSubjects.Value)(_: ExecutionContext))
        .expects("2076", ImageListSubjects.anime, *)
        .returning(EitherT.rightT(ImageList(JikanServiceSpec.testAnimeImages)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo (MAL ID: 2076)")
      searchResultContent should include ("Mon, 11 Sep 2000")
      searchResultContent should include ("Scored by 8,317 users")
      searchResultContent should include ("+ Save")

      searchResultContent should include ("class=\"glide\"")
      countOccurrences(searchResultContent, "<li class=\"glide__slide li-carousel\">") shouldBe 2
    }

    "display the anime's details when it is already saved in the database and has a single image" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED
      
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getImageList(_: String, _: ImageListSubjects.Value)(_: ExecutionContext))
        .expects("2076", ImageListSubjects.anime, *)
        .returning(EitherT.rightT(ImageList(Seq(JikanServiceSpec.kindaichiImage1))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeById("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Kindaichi Shounen no Jikenbo (MAL ID: 2076)")
      contentAsString(searchResult) should include ("Saved")
      contentAsString(searchResult) should include ("<img class=\"char-img-profile\"") // image element with no glide carousel
      contentAsString(searchResult) shouldNot include ("class=\"glide\"")
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

  /// Anime extra info ///
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
      searchResultContent should (include ("Page 1 of 1") and include ("Episodes 1 - 8 of 8"))
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

    "return a BadRequest if there is an API result but page number is not a positive integer" in {
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
      contentAsString(searchResult) should include ("API result obtained but page number is not a positive integer")
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
    "display a character's profile with multiple images" in {
      (mockJikanService.getCharacterProfile(_: String)(_: ExecutionContext))
        .expects("192285", *)
        .returning(EitherT.rightT(CharacterProfileResult(JikanServiceSpec.testCharacterProfile)))
        .once()

      (mockJikanService.getImageList(_: String, _: ImageListSubjects.Value)(_: ExecutionContext))
        .expects("192285", ImageListSubjects.characters, *)
        .returning(EitherT.rightT(ImageList(JikanServiceSpec.testCharacterImages)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getCharacterProfile("192285")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Totomaru Isshiki")
      searchResultContent should include ("<b>Nicknames:</b> Toto")
      searchResultContent should include ("Kamonohashi Ron no Kindan Suiri 2nd Season")
      countOccurrences(searchResultContent, "Main role") shouldBe 2

      searchResultContent should include ("class=\"glide\"")
      countOccurrences(searchResultContent, "<li class=\"glide__slide li-carousel\">") shouldBe 2
    }

    "display a character's profile with a single image and no biography, anime appearances or voice actor info" in {
      val characterInNoAnime: CharacterProfile = CharacterProfile(192285, JikanServiceSpec.totoImage1,
        "Character Name", Seq("Nickname 1", "Nickname 2"), 0, None, Seq(), Seq())

      (mockJikanService.getCharacterProfile(_: String)(_: ExecutionContext))
        .expects("192285", *)
        .returning(EitherT.rightT(CharacterProfileResult(characterInNoAnime)))
        .once()

      (mockJikanService.getImageList(_: String, _: ImageListSubjects.Value)(_: ExecutionContext))
        .expects("192285", ImageListSubjects.characters, *)
        .returning(EitherT.rightT(ImageList(Seq(JikanServiceSpec.totoImage1))))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getCharacterProfile("192285")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("<b>Nicknames:</b> Nickname 1, Nickname 2")
      searchResultContent should include ("<b>About:</b><br>Not available")
      searchResultContent should include ("Not appearing in any anime.")
      searchResultContent should include ("No voice actor info.")
      searchResultContent should include ("<img class=\"char-img-profile\"") // image element with no glide carousel
      searchResultContent shouldNot include ("class=\"glide\"")
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

  "ApplicationController .getAnimeRecommendations()" should {
    "list anime recommendations" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeRecommendations(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(RecommendationsResult(JikanServiceSpec.testRecommendations)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRecommendations("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent should include ("Bakemonogatari")
      searchResultContent should include ("Subete ga F ni Naru")
      searchResultContent should include ("Danganronpa: Kibou no Gakuen to Zetsubou no Koukousei The Animation")

      countOccurrences(searchResultContent, "votes") shouldBe 3
    }

    "show 'No recommendations available' if there are no results" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeRecommendations(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(RecommendationsResult(Seq())))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRecommendations("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No recommendations available")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRecommendations("abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getAnimeRelations()" should {
    "list related anime and theme songs" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getThemeSongs(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(ThemesResult(JikanServiceSpec.testThemeSongs)))
        .once()

      (mockJikanService.getRelatedAnime(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(RelationsResult(JikanServiceSpec.testRelations)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRelations("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent shouldNot include ("Adaptation") // removed non-anime relations
      countOccurrences(searchResultContent, "<tr>") shouldBe 5 // 5 rows in table including header
      countOccurrences(searchResultContent, "Side Story") shouldBe 2

      searchResultContent should (include ("Openings") and include ("Endings"))
      countOccurrences(searchResultContent, "\" by") shouldBe 18 // 7 openings, 11 endings
    }

    "show 'No related anime' and 'No theme songs'" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getThemeSongs(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(ThemesResult(
          ThemeSongs(Seq(), Seq())
        )))
        .once()

      (mockJikanService.getRelatedAnime(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(RelationsResult(Seq())))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRelations("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No related anime")
      contentAsString(searchResult) should include ("No theme songs")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("99999", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRelations("99999")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Resource does not exist")
    }
  }

  "ApplicationController .getAnimeStatistics()" should {
    "display anime statistics" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeStatistics(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(StatisticsResult(JikanServiceSpec.testAnimeStats)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeStatistics("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should include ("<b>Total members:</b> 32,924")
      searchResultContent should (include ("canvas id=\"viewersChart\"") and include ("canvas id=\"scoresChart\"")
        and include ("canvas id=\"scoresBinaryChart\"") and include ("canvas id=\"scoresPieChart\""))

      searchResultContent should include (Json.stringify(Json.toJson(JikanServiceSpec.testAnimeStats)).replace("\"","&quot;"))
      searchResultContent should include (Json.stringify(Json.toJson(JikanServiceSpec.testAnimeStats.scores)).replace("\"","&quot;"))
    }

    "show 'No scores available' if there are no scores from users" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeStatistics(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(StatisticsResult(
          AnimeStats(2, 0, 0, 0, 13827, 13829, Seq())
        )))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeStatistics("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should (include ("canvas id=\"viewersChart\"") and not include "canvas id=\"scoresChart\""
      and not include "canvas id=\"scoresBinaryChart\"" and not include "canvas id=\"scoresPieChart\"")
      contentAsString(searchResult) should include ("No scores available")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeRecommendations("abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getAnimeNews()" should {
    "list anime news entries" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeNews(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(NewsResult(JikanServiceSpec.testNewsList)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeNews("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      searchResultContent should include ("PV Collection for Mar 20 - 26")
      searchResultContent should include ("19 Aug 2016 18:36")
      searchResultContent should include ("Stark700")

      countOccurrences(searchResultContent, "View full article on MAL") shouldBe 3
    }

    "show 'No news available' if there are no results" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kubikiriData)))
        .once()

      (mockJikanService.getAnimeNews(_: String)(_: ExecutionContext))
        .expects("33263", *)
        .returning(EitherT.rightT(NewsResult(Seq())))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeNews("33263")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No news available")
    }

    "return a NotFound if the anime ID is not found" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeNews("abc")(testRequest.fakeRequest)
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "ApplicationController .getAnimeStaff()" should {
    "list anime staff" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeStaff(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(StaffResult(JikanServiceSpec.testStaffList)))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeStaff("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      val searchResultContent = contentAsString(searchResult)
      searchResultContent should include ("Kindaichi Shounen no Jikenbo")
      searchResultContent should (include ("Suwa, Michihiko") and include ("Nishio, Daisuke"))
      countOccurrences(searchResultContent, "Staff image") shouldBe 2
      countOccurrences(searchResultContent, "<li>") shouldBe 4 // positions
    }

    "show 'No staff to display' if there are no results" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      (mockJikanService.getAnimeStaff(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(StaffResult(Seq())))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeStaff("2076")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("No staff to display")
    }

    "return a BadRequest if the anime ID is invalid" in {
      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("000", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The id must be at least 1.")))
        .once()

      val searchResult: Future[Result] = TestApplicationController.getAnimeStaff("000")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Bad response from upstream: The id must be at least 1.")
    }
  }
}
