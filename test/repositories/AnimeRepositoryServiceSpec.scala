package repositories

import baseSpec.BaseSpec
import com.mongodb.client.result._
import eu.timepit.refined.auto._
import models._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import services.{AnimeRepositoryService, JikanServiceSpec}

import java.time.{Instant, OffsetDateTime}
import scala.concurrent.{ExecutionContext, Future}

class AnimeRepositoryServiceSpec extends BaseSpec with MockFactory with ScalaFutures with GuiceOneAppPerSuite {
  val mockRepoTrait: AnimeRepositoryTrait = mock[AnimeRepositoryTrait]
  implicit val executionContext: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  val testRepoService = new AnimeRepositoryService(mockRepoTrait)

  private lazy val testUpdateResult: UpdateResult = UpdateResult.acknowledged(1, 1, null)
//  private lazy val testDeleteResult: DeleteResult = DeleteResult.acknowledged(1)

  private lazy val kubikiri: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 0, None, "")

  private lazy val kubikiriUpdated: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"), "OVA", Some(8), None,
    Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 4, None, "Closed circle mystery on an island")

  private lazy val kindaichiRefreshed: SavedAnime = SavedAnime(2076, "Kindaichi Shounen no Jikenbo", Some("The File of Young Kindaichi"), "TV", Some(148), Some(1997),
    Some(7.97), Instant.parse("2024-12-18T10:01:49Z"), 148, Some(10), "Best mystery anime")

  // Only testing methods that do processing beyond calling the repository method
  "create() (version called by frontend)" should {
    "return a SavedAnime" in {
      val reqBody = Some(Map(
        "url" -> List("/anime/33263"),
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75")
      ))

      (mockRepoTrait.create(_: SavedAnime))
        .expects(*)
        .returning(Future(Right(kubikiri)))
        .once()

      whenReady(testRepoService.create(reqBody)) { result =>
        result shouldBe Right(kubikiri)
      }
    }

    "return an error from AnimeRepository" in {
      val reqBody = Some(Map(
        "url" -> List("/anime/33263"),
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75")
      ))

      (mockRepoTrait.create(_: SavedAnime))
        .expects(*)
        .returning(Future(Left(APIError.BadAPIResponse(500, "Anime has already been saved"))))
        .once()

      whenReady(testRepoService.create(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(500, "Anime has already been saved"))
      }
    }

    "return an error if a required value is missing" in {
      val reqBody = Some(Map(
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75")
      ))

      whenReady(testRepoService.create(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Missing required value"))
      }
    }

    "return an error if an incorrect data type is provided" in {
      val reqBody = Some(Map(
        "url" -> List("/anime/33263"),
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("abc"),
        "MALScore" -> List("7.75")
      ))

      whenReady(testRepoService.create(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Invalid data type"))
      }
    }
  }

  "update() (version used by form without binding)" should {
    "return an UpdateResult" in {
      val reqBody = Some(Map(
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("4"),
        "score" -> List(""),
        "notes" -> List("Closed circle mystery on an island")
      ))

      (mockRepoTrait.update(_: Int, _: SavedAnime))
        .expects(33263, kubikiriUpdated)
        .returning(Future(Right(testUpdateResult)))
        .once()

      whenReady(testRepoService.update(reqBody)) { result =>
        result shouldBe Right(testUpdateResult)
      }
    }

    "return an error from AnimeRepository" in {
      val reqBody = Some(Map(
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("4"),
        "score" -> List(""),
        "notes" -> List("Closed circle mystery on an island")
      ))

      (mockRepoTrait.update(_: Int, _: SavedAnime))
        .expects(33263, kubikiriUpdated)
        .returning(Future(Left(APIError.BadAPIResponse(404, "Anime not saved in database"))))
        .once()

      whenReady(testRepoService.update(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Anime not saved in database"))
      }
    }

    "return an error if a required value is missing" in {
      val reqBody = Some(Map(
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List(),
        "score" -> List(""),
        "notes" -> List("Closed circle mystery on an island")
      ))

      whenReady(testRepoService.update(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Missing required value"))
      }
    }

    "return an error if an incorrect data type is provided" in {
      val reqBody = Some(Map(
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75"),
        "savedAt" -> List("instant"),
        "epsWatched" -> List("4"),
        "score" -> List(""),
        "notes" -> List("Closed circle mystery on an island")
      ))

      whenReady(testRepoService.update(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Invalid data type"))
      }
    }

    "return an error if an invalid number of episodes is provided" in {
      val reqBody = Some(Map(
        "id" -> List("33263"),
        "title" -> List("Kubikiri Cycle: Aoiro Savant to Zaregotozukai"),
        "titleEnglish" -> List("The Kubikiri Cycle"),
        "type" -> List("OVA"),
        "numEpisodes" -> List("8"),
        "MALScore" -> List("7.75"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("-1"),
        "score" -> List(""),
        "notes" -> List("Closed circle mystery on an island")
      ))

      whenReady(testRepoService.update(reqBody)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Invalid data type"))
      }
    }
  }

  "refresh()" should {
    val kindaichiDataRefreshed: AnimeData = JikanServiceSpec.kindaichiData1.copy(score = Some(7.97))

    "return an UpdateResult" in {
      val reqBody = Some(Map(
        "url" -> List("/saved/2076"),
        "id" -> List("2076"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("148"),
        "score" -> List("10"),
        "notes" -> List("Best mystery anime")
      ))

      (mockRepoTrait.update(_: Int, _: SavedAnime))
        .expects(2076, kindaichiRefreshed)
        .returning(Future(Right(testUpdateResult)))
        .once()

      whenReady(testRepoService.refresh(reqBody, kindaichiDataRefreshed)) { result =>
        result shouldBe Right(testUpdateResult)
      }
    }

    "return an error from AnimeRepository" in {
      val reqBody = Some(Map(
        "url" -> List("/saved/2076"),
        "id" -> List("2076"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("148"),
        "score" -> List("10"),
        "notes" -> List("Best mystery anime")
      ))

      (mockRepoTrait.update(_: Int, _: SavedAnime))
        .expects(2076, kindaichiRefreshed)
        .returning(Future(Left(APIError.BadAPIResponse(404, "Anime not saved in database"))))
        .once()

      whenReady(testRepoService.refresh(reqBody, kindaichiDataRefreshed)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Anime not saved in database"))
      }
    }

    "return an error if a required value is missing" in {
      val reqBody = Some(Map(
        "url" -> List("/saved/2076"),
        "id" -> List("2076"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List(),
        "score" -> List("10"),
        "notes" -> List("Best mystery anime")
      ))

      whenReady(testRepoService.refresh(reqBody, kindaichiDataRefreshed)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Missing required value"))
      }
    }

    "return an error if an incorrect data type is provided" in {
      val reqBody = Some(Map(
        "url" -> List("/saved/2076"),
        "id" -> List("2076"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("abc"),
        "score" -> List("10"),
        "notes" -> List("Best mystery anime")
      ))

      whenReady(testRepoService.refresh(reqBody, kindaichiDataRefreshed)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Invalid data type"))
      }
    }

    "return an error if an invalid score is provided" in {
      val reqBody = Some(Map(
        "url" -> List("/saved/2076"),
        "id" -> List("2076"),
        "savedAt" -> List("2024-12-18T10:01:49Z"),
        "epsWatched" -> List("100"),
        "score" -> List("12"),
        "notes" -> List("Best mystery anime")
      ))

      whenReady(testRepoService.refresh(reqBody, kindaichiDataRefreshed)) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "Invalid data type"))
      }
    }
  }
}
