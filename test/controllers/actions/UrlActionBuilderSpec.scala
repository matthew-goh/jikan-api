package controllers.actions

import baseSpec.BaseSpecWithApplication
import controllers.actions.ModifiedRequests._
import org.scalamock.scalatest.MockFactory
import play.api.mvc._
import play.api.test.Helpers._

import scala.concurrent.{ExecutionContext, Future}

class UrlActionBuilderSpec extends BaseSpecWithApplication with MockFactory {
  private val bodyParser: BodyParsers.Default = stub[BodyParsers.Default]
  private val TestUrlActionBuilder = new UrlActionBuilderImpl(bodyParser)

  "UrlActionBuilder" should {
    val dummyBlock: RequestWithUrl[AnyContent] => Future[Result] = { _ =>
      Future.successful(Results.Ok)
    }

    "pass a posted URL to the block" in {
      val postedRequest = testRequest.buildPost("saveanime").withFormUrlEncodedBody("url" -> "/anime/33263")

      // dummy implementation of block() checking url
      val block: RequestWithUrl[AnyContent] => Future[Result] = req => {
        req.url shouldBe "/anime/33263"
        Future.successful(Results.Ok("Success"))
      }

      val invokeBlockResult: Future[Result] = TestUrlActionBuilder.invokeBlock(postedRequest, block)
      status(invokeBlockResult) shouldBe OK
      contentAsString(invokeBlockResult) shouldBe "Success"
    }

    "return a BadRequest if the posted URL is blank" in {
      val postedRequest = testRequest.buildPost("saveanime").withFormUrlEncodedBody("url" -> "")

      val invokeBlockResult: Future[Result] = TestUrlActionBuilder.invokeBlock(postedRequest, dummyBlock)
      status(invokeBlockResult) shouldBe BAD_REQUEST
      contentAsString(invokeBlockResult) should include ("Failed to post source url")
    }

    "return a BadRequest if the form body is missing" in {
      val postedRequest = testRequest.buildPost("saveanime")

      val invokeBlockResult: Future[Result] = TestUrlActionBuilder.invokeBlock(postedRequest, dummyBlock)
      status(invokeBlockResult) shouldBe BAD_REQUEST
      contentAsString(invokeBlockResult) should include ("Failed to post source url")
    }
  }
}
