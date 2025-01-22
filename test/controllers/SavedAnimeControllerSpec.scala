package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import eu.timepit.refined.auto._
import models._
import org.scalamock.scalatest.MockFactory
import play.api.libs.json._
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{JikanService, JikanServiceSpec}

import java.time.{Instant, OffsetDateTime}
import scala.concurrent.{ExecutionContext, Future}

class SavedAnimeControllerSpec extends BaseSpecWithApplication with MockFactory {
  val mockJikanService: JikanService = mock[JikanService]
//  val mockSaveAnimeAction: SaveAnimeAction = mock[SaveAnimeAction]
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

  private lazy val kubikiri: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 0, None, "")

  private lazy val detectiveSchoolQ: SavedAnime = SavedAnime(407, "Tantei Gakuen Q", Some("Detective School Q"), "TV", Some(45), Some(2003),
    Some(7.73), Instant.parse("2024-12-18T10:01:49Z"), 21, Some(9), "")

  private lazy val kindaichiRefreshed: SavedAnime = kindaichi.copy(MALScore = Some(7.97))

  private lazy val kubikiriUpdated: SavedAnime = kubikiri.copy(episodesWatched = 4, notes = "Closed circle mystery on an island")

  def countOccurrences(fullContent: String, target: String): Int =
    fullContent.sliding(target.length).count(window => window == target)
  
  ///// METHODS FOCUSING ON REPOSITORY /////
  "SavedAnimeController .listSavedAnime()" should {
    "list all saved anime in default order (ascending saved_at)" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Future[Result] = TestSavedAnimeController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult3: Future[Result] = TestSavedAnimeController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnime("all", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo") should be < contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
      countOccurrences(contentAsString(listingResult), "Unsave") shouldBe 3
    }

    "list all saved anime sorted by the user's score in descending order" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Future[Result] = TestSavedAnimeController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult3: Future[Result] = TestSavedAnimeController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnime("all", "score", "desc")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
      contentAsString(listingResult).indexOf("Tantei Gakuen Q") should be < contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
    }

    "show 'No saved anime' if the database is empty" in {
      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnime("all", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime.")
    }

    "show 'No saved anime' if no saved anime matches the selected completion status" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnime("not_started", "saved_at", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime.")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestSavedAnimeController.listSavedAnime("all", "?", "?")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "SavedAnimeController .sortSavedList()" should {
    "reload the saved anime page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortsaved").withFormUrlEncodedBody(
        "completionStatus" -> "completed",
        "orderBy" -> "score",
        "sortOrder" -> "asc"
      )
      val sortResult: Future[Result] = TestSavedAnimeController.sortSavedList()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/saved/status=completed/orderby=score/order=asc")
    }

    "set the sort parameters to default values if they are missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/sortsaved").withFormUrlEncodedBody(
        "sortOrder" -> "none"
      )
      val sortResult: Future[Result] = TestSavedAnimeController.sortSavedList()(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/saved/status=all/orderby=saved_at/order=none")
    }
  }

  "SavedAnimeController .listSavedAnimeFromTitleSearch()" should {
    "list saved anime whose title contains the searched title (in alphabetical order)" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED
      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(detectiveSchoolQ))
      val createdResult2: Future[Result] = TestSavedAnimeController.create()(request2)
      status(createdResult2) shouldBe CREATED
      val request3: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult3: Future[Result] = TestSavedAnimeController.create()(request3)
      status(createdResult3) shouldBe CREATED

      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnimeFromTitleSearch("ku")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("2 matching saved anime")
      contentAsString(listingResult) should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(listingResult).indexOf("Kubikiri Cycle: Aoiro Savant to Zaregotozukai") should be < contentAsString(listingResult).indexOf("Tantei Gakuen Q")
    }

    "show 'No saved anime' if no saved anime matches the searched title" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val listingResult: Future[Result] = TestSavedAnimeController.listSavedAnimeFromTitleSearch("kubikiri")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No saved anime matching the searched title.")
    }
  }

  "SavedAnimeController .searchSavedAnimeByTitle()" should {
    "redirect to saved anime title search page when a title is searched" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saved/titlesearch").withFormUrlEncodedBody(
        "title" -> "kindaichi"
      )
      val searchResult: Future[Result] = TestSavedAnimeController.searchSavedAnimeByTitle()(searchRequest)
      status(searchResult) shouldBe SEE_OTHER
      redirectLocation(searchResult) shouldBe Some("/saved/titlesearch/kindaichi")
    }

    "return a BadRequest if searched title is blank" in {
      val searchRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saved/titlesearch").withFormUrlEncodedBody(
        "title" -> ""
      )
      val searchResult: Future[Result] = TestSavedAnimeController.searchSavedAnimeByTitle()(searchRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("No search title submitted")
    }
  }

  "SavedAnimeController .viewSavedAnime()" should {
    "display the saved anime's details" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val viewResult: Future[Result] = TestSavedAnimeController.viewSavedAnime("33263")(testRequest.fakeRequest)
      status(viewResult) shouldBe OK
      contentAsString(viewResult) should include ("Kubikiri Cycle: Aoiro Savant to Zaregotozukai")
      contentAsString(viewResult) should include ("<b>Saved at:</b> 18 Dec 2024 10:01")
      contentAsString(viewResult) should include ("Not scored")
    }

    "return a NotFound if the anime is not saved in the database" in {
      val viewResult: Future[Result] = TestSavedAnimeController.viewSavedAnime("2076")(testRequest.fakeRequest)
      status(viewResult) shouldBe NOT_FOUND
      contentAsString(viewResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val viewResult: Future[Result] = TestSavedAnimeController.viewSavedAnime("abc")(testRequest.fakeRequest)
      status(viewResult) shouldBe BAD_REQUEST
      contentAsString(viewResult) should include ("Anime ID must be an integer")
    }
  }

  "SavedAnimeController .saveAnime()" should {
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
//      val saveResult = route(app, saveRequest).get
      val saveResult: Future[Result] = TestSavedAnimeController.saveAnime()(saveRequest)
      status(saveResult) shouldBe OK
      contentAsString(saveResult) should include ("Anime saved!")
//      (mockSaveAnimeAction.refine(_: Request[AnyContent]))
//        .expects(saveRequest)
//        .returning(Future.successful(Right(
//          SaveAnimeRequest("/anime/33263", kubikiri)
//        )))
//        .once()
//
//      println("Hello 1")
//      val saveResult: Future[Result] = TestSavedAnimeController.saveAnime()(saveRequest)
//      println("Hello 2")
//      status(saveResult) shouldBe OK
//      contentAsString(saveResult) should include ("Anime saved!")

      // Check that the anime is indeed saved
      val indexResult: Future[Result] = TestSavedAnimeController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      val savedAnime = contentAsJson(indexResult).as[Seq[SavedAnime]]
      savedAnime.length shouldBe 1
      savedAnime.head.MALId shouldBe 33263
    }

    "return an InternalServerError if the anime is already in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
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
      val saveResult: Future[Result] = TestSavedAnimeController.saveAnime()(saveRequest)
//      val saveResult = route(app, saveRequest).get
      status(saveResult) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(saveResult) should include ("Bad response from upstream: Anime has already been saved")
    }

    "return a BadRequest if posted source URL is missing" in {
      val saveRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "id" -> "33263"
      )
//      val saveResult = route(app, saveRequest).get
      val saveResult: Future[Result] = TestSavedAnimeController.saveAnime()(saveRequest)
      status(saveResult) shouldBe BAD_REQUEST
      contentAsString(saveResult) should include ("Failed to post source url")
    }
  }

  "SavedAnimeController .refreshSavedAnime()" should {
    "refresh an anime's MAL details in the database" in {
      val kindaichiDataRefreshed: AnimeData = JikanServiceSpec.kindaichiData1.copy(score = Some(7.97))

      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh/2076").withFormUrlEncodedBody(
        "url" -> "/saved/2076",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "148",
        "score" -> "10",
        "notes" -> "Best mystery anime"
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(kindaichiDataRefreshed)))
        .once()

      val refreshResult: Future[Result] = TestSavedAnimeController.refreshSavedAnime("2076")(refreshRequest)
      status(refreshResult) shouldBe OK
      contentAsString(refreshResult) should include ("Anime details refreshed!")

      val indexResult: Future[Result] = TestSavedAnimeController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kindaichiRefreshed)
    }

    "return a NotFound if the anime ID cannot be found in the database" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh/2076").withFormUrlEncodedBody(
        "url" -> "/saved/2076",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "epsWatched" -> "148",
        "score" -> "10",
        "notes" -> "Best mystery anime"
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("2076", *)
        .returning(EitherT.rightT(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1)))
        .once()

      val refreshResult: Future[Result] = TestSavedAnimeController.refreshSavedAnime("2076")(refreshRequest)
      status(refreshResult) shouldBe NOT_FOUND
      contentAsString(refreshResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a NotFound if the anime ID does not exist on MAL" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh/99999").withFormUrlEncodedBody(
        "url" -> "/saved/2076"
      )

      (mockJikanService.getAnimeById(_: String)(_: ExecutionContext))
        .expects("99999", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val refreshResult: Future[Result] = TestSavedAnimeController.refreshSavedAnime("99999")(refreshRequest)
      status(refreshResult) shouldBe NOT_FOUND
      contentAsString(refreshResult) should include ("Bad response from upstream: Not Found")
    }

    "return a BadRequest if posted source URL is missing" in {
      val refreshRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/refresh/2076").withFormUrlEncodedBody(
        "id" -> "2076"
      )
      val refreshResult: Future[Result] = TestSavedAnimeController.refreshSavedAnime("2076")(refreshRequest)
      status(refreshResult) shouldBe BAD_REQUEST
      contentAsString(refreshResult) should include ("Failed to post source url")
    }
  }

  "SavedAnimeController .showUpdateForm()" should {
    "display the update form with existing data filled in" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val formResult: Future[Result] = TestSavedAnimeController.showUpdateForm("2076")(testRequest.fakeRequest)
      status(formResult) shouldBe OK
      contentAsString(formResult) should include ("Kindaichi Shounen no Jikenbo")
      contentAsString(formResult) should include ("No. of episodes: 148")
      contentAsString(formResult) should include ("Best mystery anime")
    }

    "return a NotFound if the anime is not saved in the database" in {
      val formResult: Future[Result] = TestSavedAnimeController.showUpdateForm("2076")(testRequest.fakeRequest)
      status(formResult) shouldBe NOT_FOUND
      contentAsString(formResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val formResult: Future[Result] = TestSavedAnimeController.showUpdateForm("abc")(testRequest.fakeRequest)
      status(formResult) shouldBe BAD_REQUEST
      contentAsString(formResult) should include ("Anime ID must be an integer")
    }
  }

  "SavedAnimeController .updateFormSubmit()" should {
    "update anime details in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val updateRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/update/33263").withFormUrlEncodedBody(
        "MALId" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "year" -> "",
        "MALScore" -> "7.75",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "episodesWatched" -> "4",
        "score" -> "",
        "notes" -> "Closed circle mystery on an island"
      )
      val updateResult: Future[Result] = TestSavedAnimeController.updateFormSubmit("33263")(updateRequest)
      status(updateResult) shouldBe OK
      contentAsString(updateResult) should include ("Anime details updated!")

      val indexResult: Future[Result] = TestSavedAnimeController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kubikiriUpdated)
    }

    "return a NotFound if the anime is not in the database" in {
      val updateRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/update/33263").withFormUrlEncodedBody(
        "MALId" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "year" -> "",
        "MALScore" -> "7.75",
        "savedAt" -> "2024-12-18T10:01:49Z",
        "episodesWatched" -> "4",
        "score" -> "",
        "notes" -> "Closed circle mystery on an island"
      )
      val updateResult: Future[Result] = TestSavedAnimeController.updateFormSubmit("33263")(updateRequest)
      status(updateResult) shouldBe NOT_FOUND
      contentAsString(updateResult) should include ("Bad response from upstream: Anime not saved in database")
    }
  }

  "SavedAnimeController .unsaveAnime()" should {
    "delete an anime from the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val deleteResult: Future[Result] = TestSavedAnimeController.unsaveAnime("2076")(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("Anime removed from saved list")
    }

    "return a NotFound if the anime is not in the database" in {
      val deleteResult: Future[Result] = TestSavedAnimeController.unsaveAnime("2076")(FakeRequest())
      status(deleteResult) shouldBe NOT_FOUND
      contentAsString(deleteResult) should include ("Bad response from upstream: Anime not saved in database")
    }

    "return a BadRequest if the anime ID provided is not an integer" in {
      val viewResult: Future[Result] = TestSavedAnimeController.unsaveAnime("abc")(FakeRequest())
      status(viewResult) shouldBe BAD_REQUEST
      contentAsString(viewResult) should include ("Anime ID must be an integer")
    }
  }

  "SavedAnimeController .deleteAll() (test-only method)" should {
    "delete all anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Result = await(TestSavedAnimeController.create()(request))
      createdResult.header.status shouldBe CREATED

      val request2: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kubikiri))
      val createdResult2: Result = await(TestSavedAnimeController.create()(request2))
      createdResult2.header.status shouldBe CREATED

      val deleteResult: Future[Result] = TestSavedAnimeController.deleteAll()(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("All saved anime removed from database.")

      // check that database is now empty
      val indexResult: Future[Result] = TestSavedAnimeController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq()
    }

    "return the correct message if there are no anime saved in the database" in {
      val deleteResult: Future[Result] = TestSavedAnimeController.deleteAll()(FakeRequest())
      status(deleteResult) shouldBe OK
      contentAsString(deleteResult) should include ("No saved anime to delete.")
    }
  }

  ///// NON-FRONTEND METHODS, FOR TESTING /////
  "SavedAnimeController .index()" should {
    "list all anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Result = await(TestSavedAnimeController.create()(request))
      createdResult.header.status shouldBe CREATED

      val indexResult: Future[Result] = TestSavedAnimeController.index()(FakeRequest())
      status(indexResult) shouldBe OK
      contentAsJson(indexResult).as[Seq[SavedAnime]] shouldBe Seq(kindaichi)
    }
  }

  "SavedAnimeController .create()" should {
    "save an anime in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED
      contentAsJson(createdResult).as[SavedAnime] shouldBe kindaichi
    }

    "return an InternalServerError if the anime is already in the database" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe CREATED

      val duplicateRequest: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson(kindaichi))
      val duplicateResult: Future[Result] = TestSavedAnimeController.create()(duplicateRequest)
      status(duplicateResult) shouldBe INTERNAL_SERVER_ERROR
      contentAsString(duplicateResult) shouldBe "Bad response from upstream: Anime has already been saved"
    }

    "return a BadRequest if the request body could not be parsed into a SavedAnime" in {
      val request: FakeRequest[JsValue] = testRequest.buildPost("/api").withBody[JsValue](Json.toJson("abcd"))
      val createdResult: Future[Result] = TestSavedAnimeController.create()(request)
      status(createdResult) shouldBe BAD_REQUEST
      contentAsString(createdResult) shouldBe "Invalid request body"
    }
  }
}
