package controllers.actions

import baseSpec.BaseSpecWithApplication
import controllers.actions.ModifiedRequests._
import eu.timepit.refined.auto._
import models.SavedAnime
import org.scalamock.scalatest.MockFactory
import play.api.test.Helpers._

import java.time.Instant
import scala.concurrent.Future

class SaveAnimeRefinerSpec extends BaseSpecWithApplication with MockFactory {
  val TestSaveAnimeRefiner = new SaveAnimeRefiner()
  // use saveAnimeRefiner from BaseSpecWithApplication

  private lazy val kubikiri: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 0, None, "")

  "SaveAnimeRefiner" should {
    "refine a RequestWithUrl to a SaveAnimeRequest" in {
      val postedRequest = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "url" -> "/anime/33263",
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75"
      )

      val requestWithUrl = RequestWithUrl("/anime/33263", postedRequest)

      // look inside the Future returned by refine()
      saveAnimeRefiner.refine(requestWithUrl).map {
        case Right(result) =>
          result shouldBe SaveAnimeRequest(kubikiri, "/anime/33263", postedRequest)
        case _ => fail("Expected a Right")
      }
    }

    "return an error if a required value is missing" in {
      val postedRequest = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "8",
        "MALScore" -> "7.75"
      ) // missing id

      val requestWithUrl = RequestWithUrl("/anime/33263", postedRequest)

      saveAnimeRefiner.refine(requestWithUrl).map {
        case Left(result) =>
          result.header.status shouldBe BAD_REQUEST
          contentAsString(Future.successful(result)) should include("Invalid anime URL")
        case _ => fail("Expected a Left")
      }
    }

    "return an error if an incorrect data type is provided" in {
      val postedRequest = testRequest.buildPost("/saveanime").withFormUrlEncodedBody(
        "url" -> "/anime/33263",
        "id" -> "33263",
        "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
        "titleEnglish" -> "The Kubikiri Cycle",
        "type" -> "OVA",
        "numEpisodes" -> "abc",
        "MALScore" -> "7.75"
      )

      val requestWithUrl = RequestWithUrl("/anime/33263", postedRequest)

      saveAnimeRefiner.refine(requestWithUrl).map {
        case Left(result) =>
          result.header.status shouldBe BAD_REQUEST
          contentAsString(Future.successful(result)) should include("Invalid data type")
        case _ => fail("Expected a Left")
      }
    }
  }
}
