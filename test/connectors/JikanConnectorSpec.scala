package connectors

import baseSpec.BaseSpecWithApplication
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration
import models.{APIError, AnimeSearchResult}
import org.scalatest.BeforeAndAfterAll
import services.JikanServiceSpec

class JikanConnectorSpec extends BaseSpecWithApplication {
  val TestJikanConnector = new JikanConnector(ws)

  val Port = 8080
  val Host = "localhost"
  val wireMockServer = new WireMockServer(WireMockConfiguration.wireMockConfig().port(Port))

  override def beforeAll: Unit = {
    wireMockServer.start()
    configureFor(Host, Port)
  }

  override def afterAll: Unit = {
    wireMockServer.stop()
    ws.close()
  }

  "JikanConnector .get()" should {
    "return a Right(AnimeSearchResult)" in {
      stubFor(get(urlEqualTo("/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort="))
        .willReturn(aResponse()
          .withStatus(200)
          .withHeader("Content-Type", "application/json")
          .withBody(JikanServiceSpec.testAnimeSearchJsonStr)))

      whenReady(TestJikanConnector.get[AnimeSearchResult](s"http://$Host:$Port/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort=").value) { result =>
        result shouldBe Right(JikanServiceSpec.testAnimeSearchResult)
      }
    }

    "return a Not Found error" in {
      stubFor(get(urlEqualTo("/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort="))
        .willReturn(aResponse()
          .withStatus(404)
          .withHeader("Content-Type", "application/json")
          .withBody("""{"status":404,"type":"HttpException","message":"Not Found","error":null}""")))

      whenReady(TestJikanConnector.get[AnimeSearchResult](s"http://$Host:$Port/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort=").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Not Found"))
      }
    }

    "return a Bad Request error if a search parameter is invalid" in {
      stubFor(get(urlEqualTo("/searchanime/kindaichi/page=1/status=&min_score=abc&max_score=1000&order_by=&sort="))
        .willReturn(aResponse()
          .withStatus(400)
          .withHeader("Content-Type", "application/json")
          .withBody(
            """{"status":400,"type":"ValidationException",
              |"messages":{"min_score":["The min score must be a number."],"max_score":["The max score must be between 1 and 10."]},
              |"error":"Invalid or incomplete request. Make sure your request is correct. https:\/\/docs.api.jikan.moe\/"}""".stripMargin)))

      whenReady(TestJikanConnector.get[AnimeSearchResult](s"http://$Host:$Port/searchanime/kindaichi/page=1/status=&min_score=abc&max_score=1000&order_by=&sort=").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The min score must be a number. The max score must be between 1 and 10."))
      }
    }

    "return an Internal Server Error if the response is not a JSON object" in {
      stubFor(get(urlEqualTo("/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort="))
        .willReturn(aResponse()
          .withStatus(200)
          .withHeader("Content-Type", "application/json")
          .withBody("hello")))

      whenReady(TestJikanConnector.get[AnimeSearchResult](s"http://$Host:$Port/searchanime/kindaichi/page=1/status=&min_score=&max_score=&order_by=&sort=").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(500, "Could not connect"))
      }
    }
  }
}