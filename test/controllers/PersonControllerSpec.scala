package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import eu.timepit.refined.auto._
import models._
import models.people.PersonResult
import org.scalamock.scalatest.MockFactory
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.{JikanService, JikanServiceSpec}

import scala.concurrent.{ExecutionContext, Future}

class PersonControllerSpec extends BaseSpecWithApplication with MockFactory {
  val mockJikanService: JikanService = mock[JikanService]
  val TestPersonController = new PersonController(
    mockJikanService,
    component // comes from BaseSpecWithApplication
  )

  def countOccurrences(fullContent: String, target: String): Int =
    fullContent.sliding(target.length).count(window => window == target)
  
  
  "UserProfileController .getUserProfile()" should {
    "display a person's profile" in {
      (mockJikanService.getPersonProfile(_: String)(_: ExecutionContext))
        .expects("686", *)
        .returning(EitherT.rightT(PersonResult(JikanServiceSpec.testPersonProfile)))
        .once()

      val searchResult: Future[Result] = TestPersonController.getPersonProfile("686")(testRequest.fakeRequest)
      status(searchResult) shouldBe OK
      contentAsString(searchResult) should include ("Taiki Matsuno")
      contentAsString(searchResult) should include ("<b>Other names:</b> None")
      contentAsString(searchResult) should include ("<b>Birthday:</b> 16 Oct 1967")
    }

    "return a NotFound if the person is not found" in {
      (mockJikanService.getPersonProfile(_: String)(_: ExecutionContext))
        .expects("abc", *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      val searchResult: Future[Result] = TestPersonController.getPersonProfile("abc")(FakeRequest())
      status(searchResult) shouldBe NOT_FOUND
      contentAsString(searchResult) should include ("Bad response from upstream: Not Found")
    }
  }

  "PersonController .getVoicedCharacters()" should {
    "list all voiced characters in default order" in {
      (mockJikanService.getPersonProfile(_: String)(_: ExecutionContext))
        .expects("686", *)
        .returning(EitherT.rightT(PersonResult(JikanServiceSpec.testPersonProfile)))
        .once()

      val listingResult: Future[Result] = TestPersonController.getVoicedCharacters("686", "all", "none", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo Returns")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo Returns") should be < contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo</a>")
      countOccurrences(contentAsString(listingResult), "Kindaichi, Hajime") shouldBe 2
    }

    "list all voiced characters sorted by anime title in ascending order" in {
      (mockJikanService.getPersonProfile(_: String)(_: ExecutionContext))
        .expects("686", *)
        .returning(EitherT.rightT(PersonResult(JikanServiceSpec.testPersonProfile)))
        .once()

      val listingResult: Future[Result] = TestPersonController.getVoicedCharacters("686", "all", "anime", "asc")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("Kindaichi Shounen no Jikenbo</a>")
      contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo</a>") should be < contentAsString(listingResult).indexOf("Kindaichi Shounen no Jikenbo Returns")
    }

    "show 'No characters' if no voiced character matches the selected role type" in {
      (mockJikanService.getPersonProfile(_: String)(_: ExecutionContext))
        .expects("686", *)
        .returning(EitherT.rightT(PersonResult(JikanServiceSpec.testPersonProfile)))
        .once()

      val listingResult: Future[Result] = TestPersonController.getVoicedCharacters("686", "supporting", "none", "none")(testRequest.fakeRequest)
      status(listingResult) shouldBe OK
      contentAsString(listingResult) should include ("No characters.")
    }

    "return a BadRequest if the sort parameter values are invalid" in {
      val searchResult: Future[Result] = TestPersonController.getVoicedCharacters("686", "?", "?", "?")(testRequest.fakeRequest)
      status(searchResult) shouldBe BAD_REQUEST
      contentAsString(searchResult) should include ("Invalid sort parameter")
    }
  }

  "PersonController .sortVoicedCharacters()" should {
    "reload the voiced characters page when sort parameters are submitted" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/people/sortvoices/686").withFormUrlEncodedBody(
        "role" -> "main",
        "orderBy" -> "anime",
        "sortOrder" -> "desc"
      )
      val sortResult: Future[Result] = TestPersonController.sortVoicedCharacters("686")(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/people/686/voices/role=main/orderby=anime/order=desc")
    }

    "set the sort parameters to default values if they are missing from the request" in {
      val sortRequest: FakeRequest[AnyContentAsFormUrlEncoded] = testRequest.buildPost("/people/sortvoices/686").withFormUrlEncodedBody()
      val sortResult: Future[Result] = TestPersonController.sortVoicedCharacters("686")(sortRequest)
      status(sortResult) shouldBe SEE_OTHER
      redirectLocation(sortResult) shouldBe Some("/people/686/voices/role=all/orderby=none/order=none")
    }
  }
}
