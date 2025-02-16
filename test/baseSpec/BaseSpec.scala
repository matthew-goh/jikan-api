package baseSpec

import akka.stream.Materializer
import connectors.JikanConnector
import controllers.actions._
import services.{AnimeRepositoryService, JikanService}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.Application
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.Injector
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.ws.WSClient
import play.api.mvc.{AnyContentAsEmpty, MessagesControllerComponents}
import repositories.AnimeRepository

import scala.concurrent.ExecutionContext

trait BaseSpec extends AnyWordSpec with Matchers

trait BaseSpecWithApplication extends BaseSpec with GuiceOneServerPerSuite with ScalaFutures with BeforeAndAfterEach with BeforeAndAfterAll with Eventually {

  implicit val mat: Materializer = app.materializer
  implicit val executionContext: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  implicit val ws: WSClient = app.injector.instanceOf[WSClient]

  // created instances of the controller components and data repository - new instances will be made for each suite run
  lazy val component: MessagesControllerComponents = injector.instanceOf[MessagesControllerComponents]
  lazy val repoService: AnimeRepositoryService = injector.instanceOf[AnimeRepositoryService]
  lazy val repository: AnimeRepository = injector.instanceOf[AnimeRepository]
  lazy val service: JikanService = injector.instanceOf[JikanService]
  lazy val connector: JikanConnector = injector.instanceOf[JikanConnector]

  lazy val urlActionBuilder: UrlActionBuilderImpl = injector.instanceOf[UrlActionBuilderImpl]
  lazy val saveAnimeRefiner: SaveAnimeRefiner = injector.instanceOf[SaveAnimeRefiner]

  implicit val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  lazy val injector: Injector = app.injector

  // called to create app - use a separate test database
  override def fakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(Map(
        "mongodb.uri" -> "mongodb://localhost:27017/jikan-api-test"
      ))
      .build()

  // contains fakeRequest and buildGet, buildPost, etc
  protected val testRequest: TestRequest = new TestRequest(messagesApi)
}
