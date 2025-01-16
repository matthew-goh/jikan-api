package controllers

import baseSpec.BaseSpecWithApplication
import cats.data.EitherT
import eu.timepit.refined.auto._
import models._
import models.people.PersonResult
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
      contentAsString(searchResult) should include ("Has voiced characters in <b>2</b> anime")
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
}
