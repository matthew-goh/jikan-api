package services

import baseSpec.BaseSpec
import cats.data.EitherT
import connectors.JikanConnector
import models._
import models.characters._
import models.episodes._
import models.recommendations._
import models.reviews._
import models.userfavourites.{AnimeFavourite, CharacterFavourite, UserFavouritesData, UserFavouritesResult}
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json._

import java.time.{Instant, OffsetDateTime}
import scala.concurrent.ExecutionContext

class JikanServiceSpec extends BaseSpec with MockFactory with ScalaFutures with GuiceOneAppPerSuite {
  val mockConnector = mock[JikanConnector]
  implicit val executionContext: ExecutionContext = app.injector.instanceOf[ExecutionContext]
  val testService = new JikanService(mockConnector)

  "getAnimeSearchResults()" should {
    "return anime search results" in {
      (mockConnector.get[AnimeSearchResult](_: String)(_: OFormat[AnimeSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime?q=kindaichi&page=1&status=&min_score=&max_score=&order_by=&sort=", *, *) // can take *, which shows that the connector can expect any request in place of the parameter. You might sometimes see this as any().
        .returning(EitherT.rightT(Json.parse(JikanServiceSpec.testAnimeSearchJsonStr).as[AnimeSearchResult])) // explicitly states what the connector method returns
        .once() // how many times we can expect this response

      // allows for the result to be waited for as the Future type can be seen as a placeholder for a value we don't have yet
      whenReady(testService.getAnimeSearchResults(search = "kindaichi", page = "1", queryExt = "status=&min_score=&max_score=&order_by=&sort=").value) { result =>
        result shouldBe Right(JikanServiceSpec.testAnimeSearchResult)
      }
    }

    "return an error" in {
      (mockConnector.get[AnimeSearchResult](_: String)(_: OFormat[AnimeSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime?q=kindaichi&page=1&status=&min_score=aaa&max_score=&order_by=&sort=", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The min score must be a number.")))
        .once()

      whenReady(testService.getAnimeSearchResults(search = "kindaichi", page = "1", queryExt = "status=&min_score=aaa&max_score=&order_by=&sort=").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The min score must be a number."))
      }
    }
  }

  "queryExtToAnimeSearchParams()" should {
    "return an AnimeSearchParams object" in {
      testService.queryExtToAnimeSearchParams("status=&min_score=&max_score=&order_by=&sort=") shouldBe AnimeSearchParams("","","","","")
      testService.queryExtToAnimeSearchParams("status=complete&min_score=5.5&max_score=9&order_by=title&sort=desc") shouldBe AnimeSearchParams("complete","5.5","9","title","desc")
    }
  }

  "getAnimeById()" should {
    "return an anime's details" in {
      (mockConnector.get[AnimeIdSearchResult](_: String)(_: OFormat[AnimeIdSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/2076", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testAnimeIdSearchJson.as[AnimeIdSearchResult]))
        .once()

      whenReady(testService.getAnimeById("2076").value) { result =>
        result shouldBe Right(AnimeIdSearchResult(JikanServiceSpec.kindaichiData1))
      }
    }

    "return an error" in {
      (mockConnector.get[AnimeSearchResult](_: String)(_: OFormat[AnimeSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/abc", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      whenReady(testService.getAnimeById("abc").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Not Found"))
      }
    }
  }

  "getUserProfile()" should {
    "return a user's details" in {
      (mockConnector.get[UserProfileResult](_: String)(_: OFormat[UserProfileResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/users/Emotional-Yam8/full", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testUserProfileJson.as[UserProfileResult]))
        .once()

      whenReady(testService.getUserProfile("Emotional-Yam8").value) { result =>
        result shouldBe Right(UserProfileResult(JikanServiceSpec.testUserProfile))
      }
    }

    "return an error" in {
      (mockConnector.get[UserProfileResult](_: String)(_: OFormat[UserProfileResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/users/abc/full", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      whenReady(testService.getUserProfile("abc").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Resource does not exist"))
      }
    }
  }

  "getUserFavourites()" should {
    "return a user's favourites" in {
      (mockConnector.get[UserFavouritesResult](_: String)(_: OFormat[UserFavouritesResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/users/Emotional-Yam8/favorites", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testFavouritesJson.as[UserFavouritesResult]))
        .once()

      whenReady(testService.getUserFavourites("Emotional-Yam8").value) { result =>
        result shouldBe Right(UserFavouritesResult(UserFavouritesData(JikanServiceSpec.testAnimeFavourites, JikanServiceSpec.testCharacterFavourites)))
      }
    }

    "return an error" in {
      (mockConnector.get[UserFavouritesResult](_: String)(_: OFormat[UserFavouritesResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/users/abc/favorites", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Resource does not exist")))
        .once()

      whenReady(testService.getUserFavourites("abc").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Resource does not exist"))
      }
    }
  }

  "getAnimeEpisodes()" should {
    "return anime episodes" in {
      (mockConnector.get[EpisodeSearchResult](_: String)(_: OFormat[EpisodeSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/episodes?page=1", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testEpisodeSearchJson.as[EpisodeSearchResult]))
        .once()

      whenReady(testService.getAnimeEpisodes("33263", "1").value) { result =>
        result shouldBe Right(JikanServiceSpec.testEpisodeSearchResult)
      }
    }

    "return an error" in {
      (mockConnector.get[EpisodeSearchResult](_: String)(_: OFormat[EpisodeSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/episodes?page=-1", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The page must be at least 1.")))
        .once()

      whenReady(testService.getAnimeEpisodes("33263", "-1").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The page must be at least 1."))
      }
    }
  }

  "getAnimeEpisodeDetails()" should {
    "return an episode's details" in {
      (mockConnector.get[SingleEpisodeResult](_: String)(_: OFormat[SingleEpisodeResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/episodes/143", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testEpisodeDetailsJson.as[SingleEpisodeResult]))
        .once()

      whenReady(testService.getAnimeEpisodeDetails("33263", "143").value) { result =>
        result shouldBe Right(SingleEpisodeResult(JikanServiceSpec.testEpisodeDetails))
      }
    }

    "return an error" in {
      (mockConnector.get[SingleEpisodeResult](_: String)(_: OFormat[SingleEpisodeResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/episodes/0", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The episode id must be at least 1.")))
        .once()

      whenReady(testService.getAnimeEpisodeDetails("33263", "0").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The episode id must be at least 1."))
      }
    }
  }

  "getAnimeCharacters()" should {
    "return anime characters" in {
      (mockConnector.get[CharacterSearchResult](_: String)(_: OFormat[CharacterSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/characters", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testCharactersJson.as[CharacterSearchResult]))
        .once()

      whenReady(testService.getAnimeCharacters("33263").value) { result =>
        result shouldBe Right(CharacterSearchResult(JikanServiceSpec.testCharacters))
      }
    }

    "return an error" in {
      (mockConnector.get[CharacterSearchResult](_: String)(_: OFormat[CharacterSearchResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/abc/characters", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      whenReady(testService.getAnimeCharacters("abc").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Not Found"))
      }
    }
  }

  "getCharacterProfile()" should {
    "return a character profile" in {
      (mockConnector.get[CharacterProfileResult](_: String)(_: OFormat[CharacterProfileResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/characters/192285/full", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testCharacterProfileJson.as[CharacterProfileResult]))
        .once()

      whenReady(testService.getCharacterProfile("192285").value) { result =>
        result shouldBe Right(CharacterProfileResult(JikanServiceSpec.testCharacterProfile))
      }
    }

    "return an error" in {
      (mockConnector.get[CharacterProfileResult](_: String)(_: OFormat[CharacterProfileResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/characters/0/full", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The id must be at least 1.")))
        .once()

      whenReady(testService.getCharacterProfile("0").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The id must be at least 1."))
      }
    }
  }

  "getAnimeReviews()" should {
    "return anime reviews" in {
      (mockConnector.get[ReviewsResult](_: String)(_: OFormat[ReviewsResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/2076/reviews?page=1&preliminary=true&spoilers=true", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testReviewsJson.as[ReviewsResult]))
        .once()

      whenReady(testService.getAnimeReviews("2076", "1", "true", "true").value) { result =>
        result shouldBe Right(JikanServiceSpec.testReviewsResult)
      }
    }

    "return an error" in {
      (mockConnector.get[ReviewsResult](_: String)(_: OFormat[ReviewsResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/2076/reviews?page=1&preliminary=ff&spoilers=true", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(400, "The preliminary field must be true or false.")))
        .once()

      whenReady(testService.getAnimeReviews("2076", "1", "ff", "true").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(400, "The preliminary field must be true or false."))
      }
    }
  }

  "getAnimeRecommendations()" should {
    "return anime recommendations" in {
      (mockConnector.get[RecommendationsResult](_: String)(_: OFormat[RecommendationsResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/33263/recommendations", *, *)
        .returning(EitherT.rightT(JikanServiceSpec.testRecommendationsJson.as[RecommendationsResult]))
        .once()

      whenReady(testService.getAnimeRecommendations("33263").value) { result =>
        result shouldBe Right(RecommendationsResult(JikanServiceSpec.testRecommendations))
      }
    }

    "return an error" in {
      (mockConnector.get[RecommendationsResult](_: String)(_: OFormat[RecommendationsResult], _: ExecutionContext))
        .expects("https://api.jikan.moe/v4/anime/abc/recommendations", *, *)
        .returning(EitherT.leftT(APIError.BadAPIResponse(404, "Not Found")))
        .once()

      whenReady(testService.getAnimeRecommendations("abc").value) { result =>
        result shouldBe Left(APIError.BadAPIResponse(404, "Not Found"))
      }
    }
  }
}

object JikanServiceSpec {
  val testSearchPagination: AnimeSearchPagination = AnimeSearchPagination(1, 1, has_next_page = false, AnimeSearchPagItems(9, 9, 25))

  val kindaichiData1: AnimeData = AnimeData(2076,"Kindaichi Shounen no Jikenbo",Some("The File of Young Kindaichi"),"TV",Some(148),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("1997-04-07T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2000-09-11T00:00:00+00:00").toInstant)),
    Some("R - 17+ (violence & profanity)"),Some(7.94),Some(8317),
    Some("""Hajime Kindaichi's unorganized appearance and lax nature may give the impression of an average high school student, but a book should never be judged by its cover. Hajime is the grandson of the man who was once Japan's greatest detective, and he is also a remarkable sleuth himself.
           |
           |With the help of his best friend, Miyuki Nanase, and the peculiar inspector Isamu Kenmochi, Hajime travels to remote islands, ominous towns, abysmal seas, and other hostile environments. His life's mission is to uncover the truth behind some of the most cunning, grueling, and disturbing mysteries the world has ever faced.
           |
           |[Written by MAL Rewrite]""".stripMargin),List(Genre(7,"Mystery")),Some(1997), Images(JpgImage("https://cdn.myanimelist.net/images/anime/1702/120440.jpg")))

  val kindaichiData2: AnimeData = AnimeData(22817,"Kindaichi Shounen no Jikenbo Returns",Some("The File of Young Kindaichi Returns"),"TV",Some(25),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2014-04-05T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2014-09-27T00:00:00+00:00").toInstant)),
    Some("R - 17+ (violence & profanity)"),Some(7.54),Some(7902),
    Some("""High school student Hajime Kindaichi is the supposed grandson of famous private detective Kosuke Kindaichi. Visiting Hong Kong for a fashion event with Kindaichi, our hero's girlfriend Miyuki is captured by a stranger in a case of mistaken identity. The journey to save Miyuki itself leads to yet another crime case...
           |
           |(Source: YTV)""".stripMargin),List(Genre(7,"Mystery")),Some(2014), Images(JpgImage("https://cdn.myanimelist.net/images/anime/7/61271.jpg")))

  val kindaichiData3: AnimeData = AnimeData(3245,"Kindaichi Shounen no Jikenbo Specials",Some("Kindaichi Case Files Special"),"TV Special",Some(2),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2007-11-12T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2007-11-19T00:00:00+00:00").toInstant)),
    Some("PG-13 - Teens 13 or older"),Some(7.23),Some(1202),
    Some("""Kindaichi and the gang are on their way to a hot spring, but get lost and end up at a run down and sinister hotel. They are told that a vampire used to live in the hotel way back. Someone even died (was found with bite marks on the neck).
           |
           |Six years ago a girl was found in the cellar with bite marks on her neck, and the villagers killed her. When one of the guests is killed and Miyuki is attacked by a creature with fangs, it would seem like the vampire is still there...""".stripMargin),
    List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/1766/121287.jpg")))

  val kindaichiData4: AnimeData = AnimeData(31227,"Kindaichi Shounen no Jikenbo Returns 2nd Season",None,"TV",Some(22),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2015-10-03T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2016-03-26T00:00:00+00:00").toInstant)),
    Some("R - 17+ (violence & profanity)"),Some(7.66),Some(5199),
    Some("""Hajime Kindaichi once again becomes embroiled in solving baffling cases and deciphering puzzling crimes that would confound the most seasoned of detectives. Whether it's a gruesome murder and shady circumstances surrounding the Japanese board game Go; a perplexing and macabre case involving a mysterious character, "Rosenkreutz," and blue roses; or blood curdling crimes associated with an urban legend at a winter ski resort – Hajime is out to crack them all!
           |
           |(Source: YTV)""".stripMargin),
    List(Genre(7,"Mystery")),Some(2015), Images(JpgImage("https://cdn.myanimelist.net/images/anime/9/75726.jpg")))

  val kindaichiData5: AnimeData = AnimeData(2077,"Kindaichi Shounen no Jikenbo Movie 1: Operazakan - Aratanaru Satsujin",None,"Movie",Some(1),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("1996-12-14T00:00:00+00:00").toInstant),None),Some("PG-13 - Teens 13 or older"),Some(7.1),Some(1731),
    Some(s"""Invited for a anniversary celebration, Kindaichi, Miyuki and inspector Kenmochi re-visit the Opera House. There they discover that a play of "The Phantom of the Opera" is being rehearsed again. However, it doesn't take long when members of the acting troupe are killed by the "Phantom". Kindaichi will once again have to solve a murder series in the Opera House. \n\n(Source: ANN)"""),
    List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/10/25372.jpg")))

  val kindaichiData6: AnimeData = AnimeData(9154,"Kindaichi Shounen no Jikenbo Movie 2: Satsuriku no Deep Blue",
    Some("Young Kindaichi's Casebook: Deep Blue Massacre"),"Movie",Some(1),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("1999-08-21T00:00:00+00:00").toInstant),None),Some("PG-13 - Teens 13 or older"),Some(6.97),Some(782),
    Some("""The movie is an alternative version to the "Satsuriku no Deep Blue" arc of the 1997 Kindaichi TV series.
           |
           |Kindaichi, Miyuki and Fumi are invited to the resort of the Deep Blue Island by their senpai Akane, the daughter of the president of the Aizawa Group. A group of criminals infiltrate the hotel diguised as waiters to kill the members of the Aizawa Group. The criminals don't know their boss in person, and they don't know either that he's in the hotel with the members of the Aizawa Group.""".stripMargin),
    List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/1845/92957.jpg")))

  val kindaichiData7: AnimeData = AnimeData(15819,"Kindaichi Shounen no Jikenbo: Kuromajutsu Satsujin Jiken-hen",None,"OVA",Some(2),"Finished Airing",
    AirDates(Some(OffsetDateTime.parse("2012-12-17T00:00:00+00:00").toInstant),Some(OffsetDateTime.parse("2013-03-15T00:00:00+00:00").toInstant)),
    Some("R - 17+ (violence & profanity)"),Some(7.01),Some(705),
    Some("Kindaichi is back with another mystery to solve."),List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/3/45070.jpg")))

  val kindaichiData8: AnimeData = AnimeData(32376,"Kindaichi Shounen no Jikenbo Returns 2nd Season: Akechi Keibu no Jikenbo",None, "Special",Some(1),
    "Finished Airing",AirDates(Some(OffsetDateTime.parse("2015-12-26T00:00:00+00:00").toInstant),None),Some("R - 17+ (violence & profanity)"),Some(7.11),Some(923),
    Some("""The official website of the The File of Young Kindaichi Returns anime announced that a one-hour special television episode of the anime titled "The File of Inspector Akechi" (Akechi Keibu no Jikenbo) will air on December 26. The site streamed a trailer on Sunday, which previews the episode and its story. The video also reveals that Yudai Chiba will play Ryūtaro Kobayashi, a junior detective under detective Kengo Akechi.
           |
           |(Source: ANN)""".stripMargin),
    List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/9/77703.jpg")))

  val kindaichiData9: AnimeData = AnimeData(21701,"Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken",None,"Special",Some(1),
    "Finished Airing",AirDates(Some(OffsetDateTime.parse("1997-04-27T00:00:00+00:00").toInstant),None),Some("R - 17+ (violence & profanity)"),Some(6.71),Some(606),
    Some("""A one-hour special that aired after a month of the series' absence on television between episodes 23 and 24.
           |
           |Kindaichi will have to investigate in a hospital where series of murder happen.""".stripMargin),
    List(Genre(7,"Mystery")),None, Images(JpgImage("https://cdn.myanimelist.net/images/anime/1256/92959.jpg")))

  val testSearchData: Seq[AnimeData] = Seq(kindaichiData1, kindaichiData2, kindaichiData3, kindaichiData4, kindaichiData5, kindaichiData6, kindaichiData7, kindaichiData8, kindaichiData9)

  val testAnimeSearchResult: AnimeSearchResult = AnimeSearchResult(testSearchPagination, testSearchData)

  val testUserAnimeStatistics: UserAnimeStatistics = UserAnimeStatistics(21.1, 6.92, 1, 91, 4, 1, 1245)

  val testUserProfile: UserProfile = UserProfile(14084682, "Emotional-Yam8", OffsetDateTime.parse("2024-12-16T17:00:29+00:00").toInstant,
    OffsetDateTime.parse("2021-11-21T00:00:00+00:00").toInstant, None, UserStatisticsObj(testUserAnimeStatistics))

  val testAnimeFavourites: Seq[AnimeFavourite] = Seq(
    AnimeFavourite(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", "OVA", 2016, Images(JpgImage("https://cdn.myanimelist.net/images/anime/12/81588.jpg?s=96117a32acdeb4883b97db0ae9f24e13"))),
    AnimeFavourite(2076, "Kindaichi Shounen no Jikenbo", "TV", 1997, Images(JpgImage("https://cdn.myanimelist.net/images/anime/1702/120440.jpg?s=d0612b378c4a74b0bbca8588229a3975"))),
    AnimeFavourite(407, "Tantei Gakuen Q", "TV", 2003, Images(JpgImage("https://cdn.myanimelist.net/images/anime/1/407.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44")))
  )
  val testCharacterFavourites: Seq[CharacterFavourite] = Seq(
    CharacterFavourite(17650, "Kindaichi, Hajime", Images(JpgImage("https://cdn.myanimelist.net/images/characters/3/289646.jpg?s=ca273592603d81a2ccac1993d479020e"))),
    CharacterFavourite(192285, "Isshiki, Totomaru", Images(JpgImage("https://cdn.myanimelist.net/images/characters/11/516963.jpg?s=14c1c7bed8811a93ec27586ce5d281cb")))
  )

  val testSimplePagination: SimplePagination = SimplePagination(1, has_next_page = false)
  val testEpisodeList: Seq[AnimeEpisode] = Seq(
    AnimeEpisode(1, "Day 3 (1) The Savant Gathering", Some(OffsetDateTime.parse("2016-10-26T00:00:00+00:00").toInstant), Some(4.17)),
    AnimeEpisode(2, "Day 3 (2) Assembly and Arithmetic", Some(OffsetDateTime.parse("2016-11-30T00:00:00+00:00").toInstant), Some(4.42)),
    AnimeEpisode(3, "Day 4 (1) Beheading 1", Some(OffsetDateTime.parse("2017-01-25T00:00:00+00:00").toInstant), Some(4.44)),
    AnimeEpisode(4, "Day 4 (2) The 0.14 Tragedy", Some(OffsetDateTime.parse("2017-02-22T00:00:00+00:00").toInstant), Some(4.51)),
    AnimeEpisode(5, "Day 5 (1) Beheading 2", Some(OffsetDateTime.parse("2017-03-29T00:00:00+00:00").toInstant), Some(4.49)),
    AnimeEpisode(6, "Day 5 (2) Lies", Some(OffsetDateTime.parse("2017-05-31T00:00:00+00:00").toInstant), Some(4.59)),
    AnimeEpisode(7, "Episode 7", Some(OffsetDateTime.parse("2017-08-30T00:00:00+00:00").toInstant), Some(4.47)),
    AnimeEpisode(8, "Episode 8", Some(OffsetDateTime.parse("2017-09-27T00:00:00+00:00").toInstant), Some(4.38))
  )
  val testEpisodeSearchResult: EpisodeSearchResult = EpisodeSearchResult(testSimplePagination, testEpisodeList)

  val testEpisodeDetails: EpisodeFullDetails = EpisodeFullDetails(143, "Russian Dolls Murder Case File 5", Some(1440),
    Some(OffsetDateTime.parse("2000-08-07T00:00:00+09:00").toInstant), filler = false, recap = false,
    Some("""The real identity of the "Conductor" is revealed. (Source: Wikipedia)"""))

  val testCharacters: Seq[AnimeCharacter] = Seq(
    AnimeCharacter(CharacterInfo(29593, "Boku", Images(JpgImage("https://cdn.myanimelist.net/images/characters/15/317434.jpg?s=b7f89a35d49c9fe2dea566acb974c171"))), "Main", 784),
    AnimeCharacter(CharacterInfo(29594, "Kunagisa, Tomo", Images(JpgImage("https://cdn.myanimelist.net/images/characters/7/311929.jpg?s=624f081ad1ac310bc945be5a5fdd17f6"))), "Main", 371),
    AnimeCharacter(CharacterInfo(29595, "Aikawa, Jun", Images(JpgImage("https://cdn.myanimelist.net/images/characters/4/371513.jpg?s=97d1a9538f07cda0e3498b4804948cf5"))), "Supporting", 231),
    AnimeCharacter(CharacterInfo(36560, "Akagami, Iria", Images(JpgImage("https://cdn.myanimelist.net/images/characters/8/311171.jpg?s=ebb18f7088b52cf64834c6b7e688b74e"))), "Supporting", 1)
  )

  val testCharacterProfile: CharacterProfile = CharacterProfile(192285,
    Images(JpgImage("https://cdn.myanimelist.net/images/characters/11/516963.jpg")), "Totomaru Isshiki", Seq("Toto"), 26,
    Some("""Totomaru, frequently shortened to Toto, is a police detective of the Metropolitan Police Department. He is currently helping Ron Kamonohashi investigate cases by pretending to be the one who solves them.
        |
        |(Source: Ron Kamonohashi: Deranged Detective Wiki)""".stripMargin),
    Seq(AnimeAppearance("Main", AnimeAppearanceNested(53879, "Kamonohashi Ron no Kindan Suiri")), AnimeAppearance("Main", AnimeAppearanceNested(57635, "Kamonohashi Ron no Kindan Suiri 2nd Season"))),
    Seq(Voice("Japanese", VoiceActor(30853, "Enoki, Junya")))
  )

  val testReview1: AnimeReview = AnimeReview(Reviewer("MasterGhost"), OffsetDateTime.parse("2014-08-16T08:21:00+00:00").toInstant,
    """Story: 9
      |
      |The anime series of Kindaichi does not have an actual continuous story unlike Detective Conan.""".stripMargin,
    9, Seq("Recommended"), is_spoiler = false, is_preliminary = false)
  val testReview2: AnimeReview = AnimeReview(Reviewer("Welkin96"), OffsetDateTime.parse("2019-03-04T03:21:00+00:00").toInstant,
    "Test spoiler review", 10, Seq("Recommended", "Spoiler"), is_spoiler = true, is_preliminary = false)
  val testReview3: AnimeReview = AnimeReview(Reviewer("bushman66"), OffsetDateTime.parse("2022-04-09T19:37:00+00:00").toInstant,
    "Test preliminary and spoiler review", 9, Seq("Recommended", "Preliminary", "Spoiler"), is_spoiler = true, is_preliminary = true)
  val testReviewsResult: ReviewsResult = ReviewsResult(Seq(testReview1, testReview2, testReview3), testSimplePagination)

  val testRecommendations: Seq[Recommendation] = Seq(
    Recommendation(RecommendationEntry(5081, "Bakemonogatari", Images(JpgImage("https://cdn.myanimelist.net/images/anime/11/75274.jpg?s=3bb5c42c0803621dde09c52f5c4d4249"))), 16),
    Recommendation(RecommendationEntry(28621, "Subete ga F ni Naru", Images(JpgImage("https://cdn.myanimelist.net/images/anime/9/76071.jpg?s=fdc6902408ec1ded27127502ae9f0863"))), 10),
    Recommendation(RecommendationEntry(16592, "Danganronpa: Kibou no Gakuen to Zetsubou no Koukousei The Animation", Images(JpgImage("https://cdn.myanimelist.net/images/anime/4/51463.jpg?s=548e8ef2df2f9256802267ddc6cb07e9"))), 9)
  )

  val testAnimeSearchJsonStr: String =
    """
      {
      |  "pagination": {
      |    "last_visible_page": 1,
      |    "has_next_page": false,
      |    "current_page": 1,
      |    "items": {
      |      "count": 9,
      |      "total": 9,
      |      "per_page": 25
      |    }
      |  },
      |  "data": [
      |    {
      |      "mal_id": 2076,
      |      "url": "https://myanimelist.net/anime/2076/Kindaichi_Shounen_no_Jikenbo",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Les Enquetes de Kindaichi"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Young Kindaichi's Casebook"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿"
      |        },
      |        {
      |          "type": "English",
      |          "title": "The File of Young Kindaichi"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo",
      |      "title_english": "The File of Young Kindaichi",
      |      "title_japanese": "金田一少年の事件簿",
      |      "title_synonyms": [
      |        "Les Enquetes de Kindaichi",
      |        "Young Kindaichi's Casebook",
      |        "Kindaichi Case Files"
      |      ],
      |      "type": "TV",
      |      "source": "Manga",
      |      "episodes": 148,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "1997-04-07T00:00:00+00:00",
      |        "to": "2000-09-11T00:00:00+00:00",
      |        "prop": {
      |          "from": {
      |            "day": 7,
      |            "month": 4,
      |            "year": 1997
      |          },
      |          "to": {
      |            "day": 11,
      |            "month": 9,
      |            "year": 2000
      |          }
      |        },
      |        "string": "Apr 7, 1997 to Sep 11, 2000"
      |      },
      |      "duration": "24 min per ep",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 7.94,
      |      "scored_by": 8317,
      |      "rank": 747,
      |      "popularity": 4080,
      |      "members": 32862,
      |      "favorites": 265,
      |      "synopsis": "Hajime Kindaichi's unorganized appearance and lax nature may give the impression of an average high school student, but a book should never be judged by its cover. Hajime is the grandson of the man who was once Japan's greatest detective, and he is also a remarkable sleuth himself.\n\nWith the help of his best friend, Miyuki Nanase, and the peculiar inspector Isamu Kenmochi, Hajime travels to remote islands, ominous towns, abysmal seas, and other hostile environments. His life's mission is to uncover the truth behind some of the most cunning, grueling, and disturbing mysteries the world has ever faced.\n\n[Written by MAL Rewrite]",
      |      "background": "",
      |      "season": "spring",
      |      "year": 1997,
      |      "broadcast": {
      |        "day": "Mondays",
      |        "time": "19:00",
      |        "timezone": "Asia/Tokyo",
      |        "string": "Mondays at 19:00 (JST)"
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 53,
      |          "type": "anime",
      |          "name": "Dentsu",
      |          "url": "https://myanimelist.net/anime/producer/53/Dentsu"
      |        },
      |        {
      |          "mal_id": 76,
      |          "type": "anime",
      |          "name": "Yomiuri Telecasting",
      |          "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 22817,
      |      "url": "https://myanimelist.net/anime/22817/Kindaichi_Shounen_no_Jikenbo_Returns",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/7/61271.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/7/61271t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/7/61271l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/7/61271.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/7/61271t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/7/61271l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": "pCUF5QRdY-U",
      |        "url": "https://www.youtube.com/watch?v=pCUF5QRdY-U",
      |        "embed_url": "https://www.youtube.com/embed/pCUF5QRdY-U?enablejsapi=1&wmode=opaque&autoplay=1",
      |        "images": {
      |          "image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/default.jpg",
      |          "small_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/sddefault.jpg",
      |          "medium_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/mqdefault.jpg",
      |          "large_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/hqdefault.jpg",
      |          "maximum_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/maxresdefault.jpg"
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Returns"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Shounen no Jikenbo R"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files Returns"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files R"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿R【リターンズ】"
      |        },
      |        {
      |          "type": "English",
      |          "title": "The File of Young Kindaichi Returns"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Returns",
      |      "title_english": "The File of Young Kindaichi Returns",
      |      "title_japanese": "金田一少年の事件簿R【リターンズ】",
      |      "title_synonyms": [
      |        "Kindaichi Shounen no Jikenbo R",
      |        "Kindaichi Case Files Returns",
      |        "Kindaichi Case Files R"
      |      ],
      |      "type": "TV",
      |      "source": "Manga",
      |      "episodes": 25,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "2014-04-05T00:00:00+00:00",
      |        "to": "2014-09-27T00:00:00+00:00",
      |        "prop": {
      |          "from": {
      |            "day": 5,
      |            "month": 4,
      |            "year": 2014
      |          },
      |          "to": {
      |            "day": 27,
      |            "month": 9,
      |            "year": 2014
      |          }
      |        },
      |        "string": "Apr 5, 2014 to Sep 27, 2014"
      |      },
      |      "duration": "24 min per ep",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 7.54,
      |      "scored_by": 7902,
      |      "rank": 1760,
      |      "popularity": 5120,
      |      "members": 19477,
      |      "favorites": 67,
      |      "synopsis": "High school student Hajime Kindaichi is the supposed grandson of famous private detective Kosuke Kindaichi. Visiting Hong Kong for a fashion event with Kindaichi, our hero's girlfriend Miyuki is captured by a stranger in a case of mistaken identity. The journey to save Miyuki itself leads to yet another crime case...\n\n(Source: YTV)",
      |      "background": "",
      |      "season": "spring",
      |      "year": 2014,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": "Unknown"
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 76,
      |          "type": "anime",
      |          "name": "Yomiuri Telecasting",
      |          "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 3245,
      |      "url": "https://myanimelist.net/anime/3245/Kindaichi_Shounen_no_Jikenbo_Specials",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1766/121287.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1766/121287t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1766/121287l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1766/121287.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1766/121287t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1766/121287l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Specials"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Shounen no Jikenbo: Kyuuketsuki Densetsu Satsujin Jiken"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files: The Case of the Vampire Legend Murder"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Operaza Kan Saigo no Satsujin"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "The Third Opera House Murders"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿SP"
      |        },
      |        {
      |          "type": "English",
      |          "title": "Kindaichi Case Files Special"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Specials",
      |      "title_english": "Kindaichi Case Files Special",
      |      "title_japanese": "金田一少年の事件簿SP",
      |      "title_synonyms": [
      |        "Kindaichi Shounen no Jikenbo: Kyuuketsuki Densetsu Satsujin Jiken",
      |        "Kindaichi Case Files: The Case of the Vampire Legend Murder",
      |        "Operaza Kan Saigo no Satsujin",
      |        "The Third Opera House Murders"
      |      ],
      |      "type": "TV Special",
      |      "source": "Manga",
      |      "episodes": 2,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "2007-11-12T00:00:00+00:00",
      |        "to": "2007-11-19T00:00:00+00:00",
      |        "prop": {
      |          "from": {
      |            "day": 12,
      |            "month": 11,
      |            "year": 2007
      |          },
      |          "to": {
      |            "day": 19,
      |            "month": 11,
      |            "year": 2007
      |          }
      |        },
      |        "string": "Nov 12, 2007 to Nov 19, 2007"
      |      },
      |      "duration": "48 min per ep",
      |      "rating": "PG-13 - Teens 13 or older",
      |      "score": 7.23,
      |      "scored_by": 1202,
      |      "rank": 3156,
      |      "popularity": 10333,
      |      "members": 3066,
      |      "favorites": 7,
      |      "synopsis": "Kindaichi and the gang are on their way to a hot spring, but get lost and end up at a run down and sinister hotel. They are told that a vampire used to live in the hotel way back. Someone even died (was found with bite marks on the neck).\n\nSix years ago a girl was found in the cellar with bite marks on her neck, and the villagers killed her. When one of the guests is killed and Miyuki is attacked by a creature with fangs, it would seem like the vampire is still there...",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 53,
      |          "type": "anime",
      |          "name": "Dentsu",
      |          "url": "https://myanimelist.net/anime/producer/53/Dentsu"
      |        },
      |        {
      |          "mal_id": 207,
      |          "type": "anime",
      |          "name": "Magic Bus",
      |          "url": "https://myanimelist.net/anime/producer/207/Magic_Bus"
      |        },
      |        {
      |          "mal_id": 236,
      |          "type": "anime",
      |          "name": "YTV",
      |          "url": "https://myanimelist.net/anime/producer/236/YTV"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 31227,
      |      "url": "https://myanimelist.net/anime/31227/Kindaichi_Shounen_no_Jikenbo_Returns_2nd_Season",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/9/75726.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/9/75726t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/9/75726l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/9/75726.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/9/75726t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/9/75726l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": "pCUF5QRdY-U",
      |        "url": "https://www.youtube.com/watch?v=pCUF5QRdY-U",
      |        "embed_url": "https://www.youtube.com/embed/pCUF5QRdY-U?enablejsapi=1&wmode=opaque&autoplay=1",
      |        "images": {
      |          "image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/default.jpg",
      |          "small_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/sddefault.jpg",
      |          "medium_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/mqdefault.jpg",
      |          "large_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/hqdefault.jpg",
      |          "maximum_image_url": "https://img.youtube.com/vi/pCUF5QRdY-U/maxresdefault.jpg"
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Returns 2nd Season"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Shounen no Jikenbo R 2"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files Returns"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files R 2"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "The File of Young Kindaichi Returns 2nd Season"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿R【リターンズ】"
      |        },
      |        {
      |          "type": "German",
      |          "title": "The File of Young Kindaichi Returns Staffel 2"
      |        },
      |        {
      |          "type": "Spanish",
      |          "title": "Kindaichi Shounen no Jikenbo Returns Temporada 2"
      |        },
      |        {
      |          "type": "French",
      |          "title": "The File of Young Kindaichi Returns Saison 2"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Returns 2nd Season",
      |      "title_english": null,
      |      "title_japanese": "金田一少年の事件簿R【リターンズ】",
      |      "title_synonyms": [
      |        "Kindaichi Shounen no Jikenbo R 2",
      |        "Kindaichi Case Files Returns",
      |        "Kindaichi Case Files R 2",
      |        "The File of Young Kindaichi Returns 2nd Season"
      |      ],
      |      "type": "TV",
      |      "source": "Manga",
      |      "episodes": 22,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "2015-10-03T00:00:00+00:00",
      |        "to": "2016-03-26T00:00:00+00:00",
      |        "prop": {
      |          "from": {
      |            "day": 3,
      |            "month": 10,
      |            "year": 2015
      |          },
      |          "to": {
      |            "day": 26,
      |            "month": 3,
      |            "year": 2016
      |          }
      |        },
      |        "string": "Oct 3, 2015 to Mar 26, 2016"
      |      },
      |      "duration": "24 min per ep",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 7.66,
      |      "scored_by": 5199,
      |      "rank": 1366,
      |      "popularity": 6104,
      |      "members": 12399,
      |      "favorites": 30,
      |      "synopsis": "Hajime Kindaichi once again becomes embroiled in solving baffling cases and deciphering puzzling crimes that would confound the most seasoned of detectives. Whether it's a gruesome murder and shady circumstances surrounding the Japanese board game Go; a perplexing and macabre case involving a mysterious character, \"Rosenkreutz,\" and blue roses; or blood curdling crimes associated with an urban legend at a winter ski resort – Hajime is out to crack them all!\n\n(Source: YTV)",
      |      "background": "",
      |      "season": "fall",
      |      "year": 2015,
      |      "broadcast": {
      |        "day": "Saturdays",
      |        "time": "17:30",
      |        "timezone": "Asia/Tokyo",
      |        "string": "Saturdays at 17:30 (JST)"
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 76,
      |          "type": "anime",
      |          "name": "Yomiuri Telecasting",
      |          "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 2077,
      |      "url": "https://myanimelist.net/anime/2077/Kindaichi_Shounen_no_Jikenbo_Movie_1__Operazakan_-_Aratanaru_Satsujin",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/10/25372.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/10/25372t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/10/25372l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/10/25372.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/10/25372t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/10/25372l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Movie 1: Operazakan - Aratanaru Satsujin"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "The Cases of Young Kindaichi"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "The Case Files of Young Kindaichi Movie"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Shounen no Jikenbo: Operazakan - Aratanaru Satsujin"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿・オペラ座館・新たなる殺人"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Movie 1: Operazakan - Aratanaru Satsujin",
      |      "title_english": null,
      |      "title_japanese": "金田一少年の事件簿・オペラ座館・新たなる殺人",
      |      "title_synonyms": [
      |        "The Cases of Young Kindaichi",
      |        "The Case Files of Young Kindaichi Movie",
      |        "Kindaichi Shounen no Jikenbo: Operazakan - Aratanaru Satsujin"
      |      ],
      |      "type": "Movie",
      |      "source": "Light novel",
      |      "episodes": 1,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "1996-12-14T00:00:00+00:00",
      |        "to": null,
      |        "prop": {
      |          "from": {
      |            "day": 14,
      |            "month": 12,
      |            "year": 1996
      |          },
      |          "to": {
      |            "day": null,
      |            "month": null,
      |            "year": null
      |          }
      |        },
      |        "string": "Dec 14, 1996"
      |      },
      |      "duration": "1 hr 35 min",
      |      "rating": "PG-13 - Teens 13 or older",
      |      "score": 7.1,
      |      "scored_by": 1731,
      |      "rank": 3865,
      |      "popularity": 9421,
      |      "members": 4118,
      |      "favorites": 8,
      |      "synopsis": "Invited for a anniversary celebration, Kindaichi, Miyuki and inspector Kenmochi re-visit the Opera House. There they discover that a play of \"The Phantom of the Opera\" is being rehearsed again. However, it doesn't take long when members of the acting troupe are killed by the \"Phantom\". Kindaichi will once again have to solve a murder series in the Opera House. \n\n(Source: ANN)",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 53,
      |          "type": "anime",
      |          "name": "Dentsu",
      |          "url": "https://myanimelist.net/anime/producer/53/Dentsu"
      |        },
      |        {
      |          "mal_id": 76,
      |          "type": "anime",
      |          "name": "Yomiuri Telecasting",
      |          "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |        },
      |        {
      |          "mal_id": 159,
      |          "type": "anime",
      |          "name": "Kodansha",
      |          "url": "https://myanimelist.net/anime/producer/159/Kodansha"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [
      |        {
      |          "mal_id": 23,
      |          "type": "anime",
      |          "name": "School",
      |          "url": "https://myanimelist.net/anime/genre/23/School"
      |        }
      |      ],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 9154,
      |      "url": "https://myanimelist.net/anime/9154/Kindaichi_Shounen_no_Jikenbo_Movie_2__Satsuriku_no_Deep_Blue",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1845/92957.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1845/92957t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1845/92957l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1845/92957.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1845/92957t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1845/92957l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Movie 2: Satsuriku no Deep Blue"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿 殺戮のディープブルー"
      |        },
      |        {
      |          "type": "English",
      |          "title": "Young Kindaichi's Casebook: Deep Blue Massacre"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Movie 2: Satsuriku no Deep Blue",
      |      "title_english": "Young Kindaichi's Casebook: Deep Blue Massacre",
      |      "title_japanese": "金田一少年の事件簿 殺戮のディープブルー",
      |      "title_synonyms": [],
      |      "type": "Movie",
      |      "source": "Light novel",
      |      "episodes": 1,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "1999-08-21T00:00:00+00:00",
      |        "to": null,
      |        "prop": {
      |          "from": {
      |            "day": 21,
      |            "month": 8,
      |            "year": 1999
      |          },
      |          "to": {
      |            "day": null,
      |            "month": null,
      |            "year": null
      |          }
      |        },
      |        "string": "Aug 21, 1999"
      |      },
      |      "duration": "1 hr 30 min",
      |      "rating": "PG-13 - Teens 13 or older",
      |      "score": 6.97,
      |      "scored_by": 782,
      |      "rank": 4452,
      |      "popularity": 11086,
      |      "members": 2452,
      |      "favorites": 4,
      |      "synopsis": "The movie is an alternative version to the \"Satsuriku no Deep Blue\" arc of the 1997 Kindaichi TV series.\n\nKindaichi, Miyuki and Fumi are invited to the resort of the Deep Blue Island by their senpai Akane, the daughter of the president of the Aizawa Group. A group of criminals infiltrate the hotel diguised as waiters to kill the members of the Aizawa Group. The criminals don't know their boss in person, and they don't know either that he's in the hotel with the members of the Aizawa Group.",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 15819,
      |      "url": "https://myanimelist.net/anime/15819/Kindaichi_Shounen_no_Jikenbo__Kuromajutsu_Satsujin_Jiken-hen",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/3/45070.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/3/45070t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/3/45070l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/3/45070.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/3/45070t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/3/45070l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo: Kuromajutsu Satsujin Jiken-hen"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Shounen no Jikenbo (2012)"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files: Black Magic Murder Case"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿 黒魔術殺人事件編"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo: Kuromajutsu Satsujin Jiken-hen",
      |      "title_english": null,
      |      "title_japanese": "金田一少年の事件簿 黒魔術殺人事件編",
      |      "title_synonyms": [
      |        "Kindaichi Shounen no Jikenbo (2012)",
      |        "Kindaichi Case Files: Black Magic Murder Case"
      |      ],
      |      "type": "OVA",
      |      "source": "Manga",
      |      "episodes": 2,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "2012-12-17T00:00:00+00:00",
      |        "to": "2013-03-15T00:00:00+00:00",
      |        "prop": {
      |          "from": {
      |            "day": 17,
      |            "month": 12,
      |            "year": 2012
      |          },
      |          "to": {
      |            "day": 15,
      |            "month": 3,
      |            "year": 2013
      |          }
      |        },
      |        "string": "Dec 17, 2012 to Mar 15, 2013"
      |      },
      |      "duration": "27 min per ep",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 7.01,
      |      "scored_by": 705,
      |      "rank": 4321,
      |      "popularity": 11218,
      |      "members": 2357,
      |      "favorites": 5,
      |      "synopsis": "Kindaichi is back with another mystery to solve.",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 32376,
      |      "url": "https://myanimelist.net/anime/32376/Kindaichi_Shounen_no_Jikenbo_Returns_2nd_Season__Akechi_Keibu_no_Jikenbo",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/9/77703.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/9/77703t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/9/77703l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/9/77703.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/9/77703t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/9/77703l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo Returns 2nd Season: Akechi Keibu no Jikenbo"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Kindaichi Case Files Returns: The File of Inspector Akechi"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿R 明智警部の事件簿"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo Returns 2nd Season: Akechi Keibu no Jikenbo",
      |      "title_english": null,
      |      "title_japanese": "金田一少年の事件簿R 明智警部の事件簿",
      |      "title_synonyms": [
      |        "Kindaichi Case Files Returns: The File of Inspector Akechi"
      |      ],
      |      "type": "Special",
      |      "source": "Manga",
      |      "episodes": 1,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "2015-12-26T00:00:00+00:00",
      |        "to": null,
      |        "prop": {
      |          "from": {
      |            "day": 26,
      |            "month": 12,
      |            "year": 2015
      |          },
      |          "to": {
      |            "day": null,
      |            "month": null,
      |            "year": null
      |          }
      |        },
      |        "string": "Dec 26, 2015"
      |      },
      |      "duration": "47 min",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 7.11,
      |      "scored_by": 923,
      |      "rank": 3816,
      |      "popularity": 11442,
      |      "members": 2204,
      |      "favorites": 0,
      |      "synopsis": "The official website of the The File of Young Kindaichi Returns anime announced that a one-hour special television episode of the anime titled \"The File of Inspector Akechi\" (Akechi Keibu no Jikenbo) will air on December 26. The site streamed a trailer on Sunday, which previews the episode and its story. The video also reveals that Yudai Chiba will play Ryūtaro Kobayashi, a junior detective under detective Kengo Akechi.\n\n(Source: ANN)",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [
      |        {
      |          "mal_id": 76,
      |          "type": "anime",
      |          "name": "Yomiuri Telecasting",
      |          "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |        }
      |      ],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    },
      |    {
      |      "mal_id": 21701,
      |      "url": "https://myanimelist.net/anime/21701/Kindaichi_Shounen_no_Jikenbo__Shinigami_Byouin_Satsujin_Jiken",
      |      "images": {
      |        "jpg": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1256/92959.jpg",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1256/92959t.jpg",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1256/92959l.jpg"
      |        },
      |        "webp": {
      |          "image_url": "https://cdn.myanimelist.net/images/anime/1256/92959.webp",
      |          "small_image_url": "https://cdn.myanimelist.net/images/anime/1256/92959t.webp",
      |          "large_image_url": "https://cdn.myanimelist.net/images/anime/1256/92959l.webp"
      |        }
      |      },
      |      "trailer": {
      |        "youtube_id": null,
      |        "url": null,
      |        "embed_url": null,
      |        "images": {
      |          "image_url": null,
      |          "small_image_url": null,
      |          "medium_image_url": null,
      |          "large_image_url": null,
      |          "maximum_image_url": null
      |        }
      |      },
      |      "approved": true,
      |      "titles": [
      |        {
      |          "type": "Default",
      |          "title": "Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken"
      |        },
      |        {
      |          "type": "Synonym",
      |          "title": "Shinigami Hospital Murder"
      |        },
      |        {
      |          "type": "Japanese",
      |          "title": "金田一少年の事件簿 SP「死神病院殺人事件」"
      |        }
      |      ],
      |      "title": "Kindaichi Shounen no Jikenbo: Shinigami Byouin Satsujin Jiken",
      |      "title_english": null,
      |      "title_japanese": "金田一少年の事件簿 SP「死神病院殺人事件」",
      |      "title_synonyms": [
      |        "Shinigami Hospital Murder"
      |      ],
      |      "type": "Special",
      |      "source": "Manga",
      |      "episodes": 1,
      |      "status": "Finished Airing",
      |      "airing": false,
      |      "aired": {
      |        "from": "1997-04-27T00:00:00+00:00",
      |        "to": null,
      |        "prop": {
      |          "from": {
      |            "day": 27,
      |            "month": 4,
      |            "year": 1997
      |          },
      |          "to": {
      |            "day": null,
      |            "month": null,
      |            "year": null
      |          }
      |        },
      |        "string": "Apr 27, 1997"
      |      },
      |      "duration": "48 min",
      |      "rating": "R - 17+ (violence & profanity)",
      |      "score": 6.71,
      |      "scored_by": 606,
      |      "rank": 5799,
      |      "popularity": 12117,
      |      "members": 1787,
      |      "favorites": 1,
      |      "synopsis": "A one-hour special that aired after a month of the series' absence on television between episodes 23 and 24.\n\nKindaichi will have to investigate in a hospital where series of murder happen.",
      |      "background": "",
      |      "season": null,
      |      "year": null,
      |      "broadcast": {
      |        "day": null,
      |        "time": null,
      |        "timezone": null,
      |        "string": null
      |      },
      |      "producers": [],
      |      "licensors": [],
      |      "studios": [
      |        {
      |          "mal_id": 18,
      |          "type": "anime",
      |          "name": "Toei Animation",
      |          "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |        }
      |      ],
      |      "genres": [
      |        {
      |          "mal_id": 7,
      |          "type": "anime",
      |          "name": "Mystery",
      |          "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |        }
      |      ],
      |      "explicit_genres": [],
      |      "themes": [],
      |      "demographics": [
      |        {
      |          "mal_id": 27,
      |          "type": "anime",
      |          "name": "Shounen",
      |          "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |        }
      |      ]
      |    }
      |  ]
      |}""".stripMargin

  val testAnimeIdSearchJson: JsValue = Json.parse(
    """
      |{
      |  "data": {
      |    "mal_id": 2076,
      |    "url": "https://myanimelist.net/anime/2076/Kindaichi_Shounen_no_Jikenbo",
      |    "images": {
      |      "jpg": {
      |        "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.jpg",
      |        "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.jpg",
      |        "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.jpg"
      |      },
      |      "webp": {
      |        "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.webp",
      |        "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.webp",
      |        "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.webp"
      |      }
      |    },
      |    "trailer": {
      |      "youtube_id": null,
      |      "url": null,
      |      "embed_url": null,
      |      "images": {
      |        "image_url": null,
      |        "small_image_url": null,
      |        "medium_image_url": null,
      |        "large_image_url": null,
      |        "maximum_image_url": null
      |      }
      |    },
      |    "approved": true,
      |    "titles": [
      |      {
      |        "type": "Default",
      |        "title": "Kindaichi Shounen no Jikenbo"
      |      },
      |      {
      |        "type": "Synonym",
      |        "title": "Les Enquetes de Kindaichi"
      |      },
      |      {
      |        "type": "Synonym",
      |        "title": "Young Kindaichi's Casebook"
      |      },
      |      {
      |        "type": "Synonym",
      |        "title": "Kindaichi Case Files"
      |      },
      |      {
      |        "type": "Japanese",
      |        "title": "金田一少年の事件簿"
      |      },
      |      {
      |        "type": "English",
      |        "title": "The File of Young Kindaichi"
      |      }
      |    ],
      |    "title": "Kindaichi Shounen no Jikenbo",
      |    "title_english": "The File of Young Kindaichi",
      |    "title_japanese": "金田一少年の事件簿",
      |    "title_synonyms": [
      |      "Les Enquetes de Kindaichi",
      |      "Young Kindaichi's Casebook",
      |      "Kindaichi Case Files"
      |    ],
      |    "type": "TV",
      |    "source": "Manga",
      |    "episodes": 148,
      |    "status": "Finished Airing",
      |    "airing": false,
      |    "aired": {
      |      "from": "1997-04-07T00:00:00+00:00",
      |      "to": "2000-09-11T00:00:00+00:00",
      |      "prop": {
      |        "from": {
      |          "day": 7,
      |          "month": 4,
      |          "year": 1997
      |        },
      |        "to": {
      |          "day": 11,
      |          "month": 9,
      |          "year": 2000
      |        }
      |      },
      |      "string": "Apr 7, 1997 to Sep 11, 2000"
      |    },
      |    "duration": "24 min per ep",
      |    "rating": "R - 17+ (violence & profanity)",
      |    "score": 7.94,
      |    "scored_by": 8317,
      |    "rank": 747,
      |    "popularity": 4080,
      |    "members": 32862,
      |    "favorites": 265,
      |    "synopsis": "Hajime Kindaichi's unorganized appearance and lax nature may give the impression of an average high school student, but a book should never be judged by its cover. Hajime is the grandson of the man who was once Japan's greatest detective, and he is also a remarkable sleuth himself.\n\nWith the help of his best friend, Miyuki Nanase, and the peculiar inspector Isamu Kenmochi, Hajime travels to remote islands, ominous towns, abysmal seas, and other hostile environments. His life's mission is to uncover the truth behind some of the most cunning, grueling, and disturbing mysteries the world has ever faced.\n\n[Written by MAL Rewrite]",
      |    "background": "",
      |    "season": "spring",
      |    "year": 1997,
      |    "broadcast": {
      |      "day": "Mondays",
      |      "time": "19:00",
      |      "timezone": "Asia/Tokyo",
      |      "string": "Mondays at 19:00 (JST)"
      |    },
      |    "producers": [
      |      {
      |        "mal_id": 53,
      |        "type": "anime",
      |        "name": "Dentsu",
      |        "url": "https://myanimelist.net/anime/producer/53/Dentsu"
      |      },
      |      {
      |        "mal_id": 76,
      |        "type": "anime",
      |        "name": "Yomiuri Telecasting",
      |        "url": "https://myanimelist.net/anime/producer/76/Yomiuri_Telecasting"
      |      }
      |    ],
      |    "licensors": [],
      |    "studios": [
      |      {
      |        "mal_id": 18,
      |        "type": "anime",
      |        "name": "Toei Animation",
      |        "url": "https://myanimelist.net/anime/producer/18/Toei_Animation"
      |      }
      |    ],
      |    "genres": [
      |      {
      |        "mal_id": 7,
      |        "type": "anime",
      |        "name": "Mystery",
      |        "url": "https://myanimelist.net/anime/genre/7/Mystery"
      |      }
      |    ],
      |    "explicit_genres": [],
      |    "themes": [],
      |    "demographics": [
      |      {
      |        "mal_id": 27,
      |        "type": "anime",
      |        "name": "Shounen",
      |        "url": "https://myanimelist.net/anime/genre/27/Shounen"
      |      }
      |    ]
      |  }
      |}
      |""".stripMargin)

  val testUserProfileJson: JsValue = Json.parse(
    """
      |{
      |  "data": {
      |    "mal_id": 14084682,
      |    "username": "Emotional-Yam8",
      |    "url": "https://myanimelist.net/profile/Emotional-Yam8",
      |    "images": {
      |      "jpg": {
      |        "image_url": null
      |      },
      |      "webp": {
      |        "image_url": ""
      |      }
      |    },
      |    "last_online": "2024-12-16T17:00:29+00:00",
      |    "gender": "Male",
      |    "birthday": null,
      |    "location": null,
      |    "joined": "2021-11-21T00:00:00+00:00",
      |    "statistics": {
      |      "anime": {
      |        "days_watched": 21.1,
      |        "mean_score": 6.92,
      |        "watching": 1,
      |        "completed": 91,
      |        "on_hold": 2,
      |        "dropped": 4,
      |        "plan_to_watch": 1,
      |        "total_entries": 99,
      |        "rewatched": 0,
      |        "episodes_watched": 1245
      |      },
      |      "manga": {
      |        "days_read": 0,
      |        "mean_score": 0,
      |        "reading": 0,
      |        "completed": 0,
      |        "on_hold": 0,
      |        "dropped": 0,
      |        "plan_to_read": 0,
      |        "total_entries": 0,
      |        "reread": 0,
      |        "chapters_read": 0,
      |        "volumes_read": 0
      |      }
      |    },
      |    "favorites": {
      |      "anime": [
      |        {
      |          "mal_id": 12189,
      |          "url": "https://myanimelist.net/anime/12189/Hyouka",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/13/50521.jpg?s=722592f36cc4878ddafd3dee17e06eed",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/13/50521t.jpg?s=722592f36cc4878ddafd3dee17e06eed",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/13/50521l.jpg?s=722592f36cc4878ddafd3dee17e06eed"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/13/50521.webp?s=722592f36cc4878ddafd3dee17e06eed",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/13/50521t.webp?s=722592f36cc4878ddafd3dee17e06eed",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/13/50521l.webp?s=722592f36cc4878ddafd3dee17e06eed"
      |            }
      |          },
      |          "title": "Hyouka",
      |          "type": "TV",
      |          "start_year": 2012
      |        },
      |        {
      |          "mal_id": 2204,
      |          "url": "https://myanimelist.net/anime/2204/Karakuri_Zoushi_Ayatsuri_Sakon",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/10/16022.jpg?s=9460b39228beeefbaa7ddbc2894a73e6",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/10/16022t.jpg?s=9460b39228beeefbaa7ddbc2894a73e6",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/10/16022l.jpg?s=9460b39228beeefbaa7ddbc2894a73e6"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/10/16022.webp?s=9460b39228beeefbaa7ddbc2894a73e6",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/10/16022t.webp?s=9460b39228beeefbaa7ddbc2894a73e6",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/10/16022l.webp?s=9460b39228beeefbaa7ddbc2894a73e6"
      |            }
      |          },
      |          "title": "Karakuri Zoushi Ayatsuri Sakon",
      |          "type": "TV",
      |          "start_year": 1999
      |        },
      |        {
      |          "mal_id": 30015,
      |          "url": "https://myanimelist.net/anime/30015/ReLIFE",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/3/82149.jpg?s=20b502ad13710f0919e78e3f06687f68",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/3/82149t.jpg?s=20b502ad13710f0919e78e3f06687f68",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/3/82149l.jpg?s=20b502ad13710f0919e78e3f06687f68"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/3/82149.webp?s=20b502ad13710f0919e78e3f06687f68",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/3/82149t.webp?s=20b502ad13710f0919e78e3f06687f68",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/3/82149l.webp?s=20b502ad13710f0919e78e3f06687f68"
      |            }
      |          },
      |          "title": "ReLIFE",
      |          "type": "TV",
      |          "start_year": 2016
      |        },
      |        {
      |          "mal_id": 33263,
      |          "url": "https://myanimelist.net/anime/33263/Kubikiri_Cycle__Aoiro_Savant_to_Zaregotozukai",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/12/81588.jpg?s=96117a32acdeb4883b97db0ae9f24e13",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/12/81588t.jpg?s=96117a32acdeb4883b97db0ae9f24e13",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/12/81588l.jpg?s=96117a32acdeb4883b97db0ae9f24e13"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/12/81588.webp?s=96117a32acdeb4883b97db0ae9f24e13",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/12/81588t.webp?s=96117a32acdeb4883b97db0ae9f24e13",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/12/81588l.webp?s=96117a32acdeb4883b97db0ae9f24e13"
      |            }
      |          },
      |          "title": "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
      |          "type": "OVA",
      |          "start_year": 2016
      |        },
      |        {
      |          "mal_id": 2076,
      |          "url": "https://myanimelist.net/anime/2076/Kindaichi_Shounen_no_Jikenbo",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.jpg?s=d0612b378c4a74b0bbca8588229a3975",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.jpg?s=d0612b378c4a74b0bbca8588229a3975",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.jpg?s=d0612b378c4a74b0bbca8588229a3975"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.webp?s=d0612b378c4a74b0bbca8588229a3975",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.webp?s=d0612b378c4a74b0bbca8588229a3975",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.webp?s=d0612b378c4a74b0bbca8588229a3975"
      |            }
      |          },
      |          "title": "Kindaichi Shounen no Jikenbo",
      |          "type": "TV",
      |          "start_year": 1997
      |        },
      |        {
      |          "mal_id": 407,
      |          "url": "https://myanimelist.net/anime/407/Tantei_Gakuen_Q",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1/407.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1/407t.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1/407l.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1/407.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1/407t.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1/407l.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44"
      |            }
      |          },
      |          "title": "Tantei Gakuen Q",
      |          "type": "TV",
      |          "start_year": 2003
      |        }
      |      ],
      |      "manga": [],
      |      "characters": [],
      |      "people": []
      |    },
      |    "updates": null,
      |    "about": null,
      |    "external": null
      |  }
      |}
      |""".stripMargin)

  val testFavouritesJson: JsValue = Json.parse(
    """{
      |  "data": {
      |    "anime": [
      |      {
      |        "mal_id": 33263,
      |        "url": "https://myanimelist.net/anime/33263/Kubikiri_Cycle__Aoiro_Savant_to_Zaregotozukai",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/12/81588.jpg?s=96117a32acdeb4883b97db0ae9f24e13",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/12/81588t.jpg?s=96117a32acdeb4883b97db0ae9f24e13",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/12/81588l.jpg?s=96117a32acdeb4883b97db0ae9f24e13"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/12/81588.webp?s=96117a32acdeb4883b97db0ae9f24e13",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/12/81588t.webp?s=96117a32acdeb4883b97db0ae9f24e13",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/12/81588l.webp?s=96117a32acdeb4883b97db0ae9f24e13"
      |          }
      |        },
      |        "title": "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
      |        "type": "OVA",
      |        "start_year": 2016
      |      },
      |      {
      |        "mal_id": 2076,
      |        "url": "https://myanimelist.net/anime/2076/Kindaichi_Shounen_no_Jikenbo",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.jpg?s=d0612b378c4a74b0bbca8588229a3975",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.jpg?s=d0612b378c4a74b0bbca8588229a3975",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.jpg?s=d0612b378c4a74b0bbca8588229a3975"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/1702/120440.webp?s=d0612b378c4a74b0bbca8588229a3975",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440t.webp?s=d0612b378c4a74b0bbca8588229a3975",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/1702/120440l.webp?s=d0612b378c4a74b0bbca8588229a3975"
      |          }
      |        },
      |        "title": "Kindaichi Shounen no Jikenbo",
      |        "type": "TV",
      |        "start_year": 1997
      |      },
      |      {
      |        "mal_id": 407,
      |        "url": "https://myanimelist.net/anime/407/Tantei_Gakuen_Q",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/1/407.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/1/407t.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/1/407l.jpg?s=9f41e7b4e410ae8bd9dbbd307e85bb44"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/1/407.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/1/407t.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/1/407l.webp?s=9f41e7b4e410ae8bd9dbbd307e85bb44"
      |          }
      |        },
      |        "title": "Tantei Gakuen Q",
      |        "type": "TV",
      |        "start_year": 2003
      |      }
      |    ],
      |    "manga": [],
      |    "characters": [
      |      {
      |        "mal_id": 17650,
      |        "url": "https://myanimelist.net/character/17650/Hajime_Kindaichi",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/3/289646.jpg?s=ca273592603d81a2ccac1993d479020e"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/3/289646.webp?s=ca273592603d81a2ccac1993d479020e",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/3/289646t.webp?s=ca273592603d81a2ccac1993d479020e"
      |          }
      |        },
      |        "name": "Kindaichi, Hajime"
      |      },
      |      {
      |        "mal_id": 192285,
      |        "url": "https://myanimelist.net/character/192285/Totomaru_Isshiki",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/11/516963.jpg?s=14c1c7bed8811a93ec27586ce5d281cb"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/11/516963.webp?s=14c1c7bed8811a93ec27586ce5d281cb",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/11/516963t.webp?s=14c1c7bed8811a93ec27586ce5d281cb"
      |          }
      |        },
      |        "name": "Isshiki, Totomaru"
      |      }
      |    ],
      |    "people": []
      |  }
      |}
      |""".stripMargin)

  val testEpisodeSearchJson: JsValue = Json.parse(
    """{
      |  "pagination": {
      |    "last_visible_page": 1,
      |    "has_next_page": false
      |  },
      |  "data": [
      |    {
      |      "mal_id": 1,
      |      "url": null,
      |      "title": "Day 3 (1) The Savant Gathering",
      |      "title_japanese": "1) サヴァンの群青",
      |      "title_romanji": "Mikkame (1) Savant no Gunjou (三日目",
      |      "aired": "2016-10-26T00:00:00+00:00",
      |      "score": 4.17,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1563915"
      |    },
      |    {
      |      "mal_id": 2,
      |      "url": null,
      |      "title": "Day 3 (2) Assembly and Arithmetic",
      |      "title_japanese": "2) 集合と算数",
      |      "title_romanji": "Mikkame (2) Shuugou to Sansuu (三日目",
      |      "aired": "2016-11-30T00:00:00+00:00",
      |      "score": 4.42,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1572927"
      |    },
      |    {
      |      "mal_id": 3,
      |      "url": null,
      |      "title": "Day 4 (1) Beheading 1",
      |      "title_japanese": "1) 首斬り一つ",
      |      "title_romanji": "Yokkame (1) Kubikiri Hitotsu (四日目",
      |      "aired": "2017-01-25T00:00:00+00:00",
      |      "score": 4.44,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1587100"
      |    },
      |    {
      |      "mal_id": 4,
      |      "url": null,
      |      "title": "Day 4 (2) The 0.14 Tragedy",
      |      "title_japanese": "2) 0.14の悲劇",
      |      "title_romanji": "Yokkame (2) 0.14 no Higeki (四日目",
      |      "aired": "2017-02-22T00:00:00+00:00",
      |      "score": 4.51,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1593945"
      |    },
      |    {
      |      "mal_id": 5,
      |      "url": null,
      |      "title": "Day 5 (1) Beheading 2",
      |      "title_japanese": "1) 首斬り二つ",
      |      "title_romanji": "Itsukame (1) Kubikiri Futatsu (五日目",
      |      "aired": "2017-03-29T00:00:00+00:00",
      |      "score": 4.49,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1602568"
      |    },
      |    {
      |      "mal_id": 6,
      |      "url": null,
      |      "title": "Day 5 (2) Lies",
      |      "title_japanese": "2) 嘘",
      |      "title_romanji": "Itsukame (2) Uso (五日目",
      |      "aired": "2017-05-31T00:00:00+00:00",
      |      "score": 4.59,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1620331"
      |    },
      |    {
      |      "mal_id": 7,
      |      "url": null,
      |      "title": "Episode 7",
      |      "title_japanese": "3) 鴉の濡れ羽",
      |      "title_romanji": "Itsukame (3) Karasu no Nureba (五日目",
      |      "aired": "2017-08-30T00:00:00+00:00",
      |      "score": 4.47,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1657490"
      |    },
      |    {
      |      "mal_id": 8,
      |      "url": null,
      |      "title": "Episode 8",
      |      "title_japanese": null,
      |      "title_romanji": null,
      |      "aired": "2017-09-27T00:00:00+00:00",
      |      "score": 4.38,
      |      "filler": false,
      |      "recap": false,
      |      "forum_url": "https://myanimelist.net/forum/?topicid=1668602"
      |    }
      |  ]
      |}""".stripMargin
  )

  val testEpisodeDetailsJson: JsValue = Json.parse(
    """{
      |  "data": {
      |    "mal_id": 143,
      |    "url": "https://myanimelist.net/anime/2076/Kindaichi_Shounen_no_Jikenbo/episode/143",
      |    "title": "Russian Dolls Murder Case File 5",
      |    "title_japanese": "「露西亜人形殺人事件」ファイル5",
      |    "title_romanji": "Russia Ningyou Satsujin Jiken File 5",
      |    "duration": 1440,
      |    "aired": "2000-08-07T00:00:00+09:00",
      |    "filler": false,
      |    "recap": false,
      |    "synopsis": "The real identity of the \"Conductor\" is revealed. (Source: Wikipedia)"
      |  }
      |}""".stripMargin
  )

  val testCharactersJson: JsValue = Json.parse(
    """{
      |  "data": [
      |    {
      |      "character": {
      |        "mal_id": 29593,
      |        "url": "https://myanimelist.net/character/29593/Boku",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/15/317434.jpg?s=b7f89a35d49c9fe2dea566acb974c171"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/15/317434.webp?s=b7f89a35d49c9fe2dea566acb974c171",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/15/317434t.webp?s=b7f89a35d49c9fe2dea566acb974c171"
      |          }
      |        },
      |        "name": "Boku"
      |      },
      |      "role": "Main",
      |      "favorites": 784,
      |      "voice_actors": [
      |        {
      |          "person": {
      |            "mal_id": 672,
      |            "url": "https://myanimelist.net/people/672/Yuuki_Kaji",
      |            "images": {
      |              "jpg": {
      |                "image_url": "https://cdn.myanimelist.net/images/voiceactors/2/66416.jpg?s=91e56f66a0be72a89dff77e0d8ec55ce"
      |              }
      |            },
      |            "name": "Kaji, Yuuki"
      |          },
      |          "language": "Japanese"
      |        }
      |      ]
      |    },
      |    {
      |      "character": {
      |        "mal_id": 29594,
      |        "url": "https://myanimelist.net/character/29594/Tomo_Kunagisa",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/7/311929.jpg?s=624f081ad1ac310bc945be5a5fdd17f6"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/7/311929.webp?s=624f081ad1ac310bc945be5a5fdd17f6",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/7/311929t.webp?s=624f081ad1ac310bc945be5a5fdd17f6"
      |          }
      |        },
      |        "name": "Kunagisa, Tomo"
      |      },
      |      "role": "Main",
      |      "favorites": 371,
      |      "voice_actors": [
      |        {
      |          "person": {
      |            "mal_id": 6686,
      |            "url": "https://myanimelist.net/people/6686/Aoi_Yuuki",
      |            "images": {
      |              "jpg": {
      |                "image_url": "https://cdn.myanimelist.net/images/voiceactors/3/67808.jpg?s=3074a08319fa6f05424eed1f508e2233"
      |              }
      |            },
      |            "name": "Yuuki, Aoi"
      |          },
      |          "language": "Japanese"
      |        }
      |      ]
      |    },
      |    {
      |      "character": {
      |        "mal_id": 29595,
      |        "url": "https://myanimelist.net/character/29595/Jun_Aikawa",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/4/371513.jpg?s=97d1a9538f07cda0e3498b4804948cf5"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/4/371513.webp?s=97d1a9538f07cda0e3498b4804948cf5",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/4/371513t.webp?s=97d1a9538f07cda0e3498b4804948cf5"
      |          }
      |        },
      |        "name": "Aikawa, Jun"
      |      },
      |      "role": "Supporting",
      |      "favorites": 231,
      |      "voice_actors": [
      |        {
      |          "person": {
      |            "mal_id": 428,
      |            "url": "https://myanimelist.net/people/428/Yuuko_Kaida",
      |            "images": {
      |              "jpg": {
      |                "image_url": "https://cdn.myanimelist.net/images/voiceactors/3/74397.jpg?s=cb515aed18f4c63ccea72f4060617258"
      |              }
      |            },
      |            "name": "Kaida, Yuuko"
      |          },
      |          "language": "Japanese"
      |        }
      |      ]
      |    },
      |    {
      |      "character": {
      |        "mal_id": 36560,
      |        "url": "https://myanimelist.net/character/36560/Iria_Akagami",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/8/311171.jpg?s=ebb18f7088b52cf64834c6b7e688b74e"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/characters/8/311171.webp?s=ebb18f7088b52cf64834c6b7e688b74e",
      |            "small_image_url": "https://cdn.myanimelist.net/images/characters/8/311171t.webp?s=ebb18f7088b52cf64834c6b7e688b74e"
      |          }
      |        },
      |        "name": "Akagami, Iria"
      |      },
      |      "role": "Supporting",
      |      "favorites": 1,
      |      "voice_actors": [
      |        {
      |          "person": {
      |            "mal_id": 655,
      |            "url": "https://myanimelist.net/people/655/Mariya_Ise",
      |            "images": {
      |              "jpg": {
      |                "image_url": "https://cdn.myanimelist.net/images/voiceactors/2/65504.jpg?s=79bb2f4740b9c846b3c2bed12c193b4b"
      |              }
      |            },
      |            "name": "Ise, Mariya"
      |          },
      |          "language": "Japanese"
      |        }
      |      ]
      |    }
      |  ]
      |}""".stripMargin
  )

  val testCharacterProfileJson: JsValue = Json.parse(
    """
      |{
      |  "data": {
      |    "mal_id": 192285,
      |    "url": "https://myanimelist.net/character/192285/Totomaru_Isshiki",
      |    "images": {
      |      "jpg": {
      |        "image_url": "https://cdn.myanimelist.net/images/characters/11/516963.jpg"
      |      },
      |      "webp": {
      |        "image_url": "https://cdn.myanimelist.net/images/characters/11/516963.webp",
      |        "small_image_url": "https://cdn.myanimelist.net/images/characters/11/516963t.webp"
      |      }
      |    },
      |    "name": "Totomaru Isshiki",
      |    "name_kanji": "一色 都々丸",
      |    "nicknames": [
      |      "Toto"
      |    ],
      |    "favorites": 26,
      |    "about": "Totomaru, frequently shortened to Toto, is a police detective of the Metropolitan Police Department. He is currently helping Ron Kamonohashi investigate cases by pretending to be the one who solves them.\n\n(Source: Ron Kamonohashi: Deranged Detective Wiki)",
      |    "anime": [
      |      {
      |        "role": "Main",
      |        "anime": {
      |          "mal_id": 53879,
      |          "url": "https://myanimelist.net/anime/53879/Kamonohashi_Ron_no_Kindan_Suiri",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1799/137123.jpg?s=a086d60e87f039225ca8a5af44a01b93",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1799/137123t.jpg?s=a086d60e87f039225ca8a5af44a01b93",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1799/137123l.jpg?s=a086d60e87f039225ca8a5af44a01b93"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1799/137123.webp?s=a086d60e87f039225ca8a5af44a01b93",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1799/137123t.webp?s=a086d60e87f039225ca8a5af44a01b93",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1799/137123l.webp?s=a086d60e87f039225ca8a5af44a01b93"
      |            }
      |          },
      |          "title": "Kamonohashi Ron no Kindan Suiri"
      |        }
      |      },
      |      {
      |        "role": "Main",
      |        "anime": {
      |          "mal_id": 57635,
      |          "url": "https://myanimelist.net/anime/57635/Kamonohashi_Ron_no_Kindan_Suiri_2nd_Season",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1917/144334.jpg?s=0ca422cabe591f4850cb7408ced1f580",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1917/144334t.jpg?s=0ca422cabe591f4850cb7408ced1f580",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1917/144334l.jpg?s=0ca422cabe591f4850cb7408ced1f580"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/anime/1917/144334.webp?s=0ca422cabe591f4850cb7408ced1f580",
      |              "small_image_url": "https://cdn.myanimelist.net/images/anime/1917/144334t.webp?s=0ca422cabe591f4850cb7408ced1f580",
      |              "large_image_url": "https://cdn.myanimelist.net/images/anime/1917/144334l.webp?s=0ca422cabe591f4850cb7408ced1f580"
      |            }
      |          },
      |          "title": "Kamonohashi Ron no Kindan Suiri 2nd Season"
      |        }
      |      }
      |    ],
      |    "manga": [
      |      {
      |        "role": "Main",
      |        "manga": {
      |          "mal_id": 130392,
      |          "url": "https://myanimelist.net/manga/130392/Kamonohashi_Ron_no_Kindan_Suiri",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/manga/2/240565.jpg?s=1735a8e43bfd6e6568c399e75011773e",
      |              "small_image_url": "https://cdn.myanimelist.net/images/manga/2/240565t.jpg?s=1735a8e43bfd6e6568c399e75011773e",
      |              "large_image_url": "https://cdn.myanimelist.net/images/manga/2/240565l.jpg?s=1735a8e43bfd6e6568c399e75011773e"
      |            },
      |            "webp": {
      |              "image_url": "https://cdn.myanimelist.net/images/manga/2/240565.webp?s=1735a8e43bfd6e6568c399e75011773e",
      |              "small_image_url": "https://cdn.myanimelist.net/images/manga/2/240565t.webp?s=1735a8e43bfd6e6568c399e75011773e",
      |              "large_image_url": "https://cdn.myanimelist.net/images/manga/2/240565l.webp?s=1735a8e43bfd6e6568c399e75011773e"
      |            }
      |          },
      |          "title": "Kamonohashi Ron no Kindan Suiri"
      |        }
      |      }
      |    ],
      |    "voices": [
      |      {
      |        "person": {
      |          "mal_id": 30853,
      |          "url": "https://myanimelist.net/people/30853/Junya_Enoki",
      |          "images": {
      |            "jpg": {
      |              "image_url": "https://cdn.myanimelist.net/images/voiceactors/2/62840.jpg"
      |            }
      |          },
      |          "name": "Enoki, Junya"
      |        },
      |        "language": "Japanese"
      |      }
      |    ]
      |  }
      |}
      |""".stripMargin)

  val testReviewsJson: JsValue = Json.parse(
    """
      |{
      |  "pagination": {
      |    "last_visible_page": 1,
      |    "has_next_page": false
      |  },
      |  "data": [
      |    {
      |      "mal_id": 156761,
      |      "url": "https://myanimelist.net/reviews.php?id=156761",
      |      "type": "anime",
      |      "reactions": {
      |        "overall": 77,
      |        "nice": 74,
      |        "love_it": 1,
      |        "funny": 0,
      |        "confusing": 0,
      |        "informative": 2,
      |        "well_written": 0,
      |        "creative": 0
      |      },
      |      "date": "2014-08-16T08:21:00+00:00",
      |      "review": "Story: 9\n\nThe anime series of Kindaichi does not have an actual continuous story unlike Detective Conan.",
      |      "score": 9,
      |      "tags": [
      |        "Recommended"
      |      ],
      |      "is_spoiler": false,
      |      "is_preliminary": false,
      |      "episodes_watched": null,
      |      "user": {
      |        "url": "https://myanimelist.net/profile/MasterGhost",
      |        "username": "MasterGhost",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/d39f7d9d-d9b7-4855-aeb2-f035a891ba26_225w?s=c8f7183aa9ad5fde6e89f493c54b15ae"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/d39f7d9d-d9b7-4855-aeb2-f035a891ba26_225w?s=c8f7183aa9ad5fde6e89f493c54b15ae"
      |          }
      |        }
      |      }
      |    },
      |    {
      |      "mal_id": 302878,
      |      "url": "https://myanimelist.net/reviews.php?id=302878",
      |      "type": "anime",
      |      "reactions": {
      |        "overall": 25,
      |        "nice": 25,
      |        "love_it": 0,
      |        "funny": 0,
      |        "confusing": 0,
      |        "informative": 0,
      |        "well_written": 0,
      |        "creative": 0
      |      },
      |      "date": "2019-03-04T03:21:00+00:00",
      |      "review": "Test spoiler review",
      |      "score": 10,
      |      "tags": [
      |        "Recommended", "Spoiler"
      |      ],
      |      "is_spoiler": true,
      |      "is_preliminary": false,
      |      "episodes_watched": null,
      |      "user": {
      |        "url": "https://myanimelist.net/profile/Welkin96",
      |        "username": "Welkin96",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/3c9b8b8f-59f8-4dd1-a58f-2704b9d957b6_225w?s=ea212f03a5bda244f774d5429bbb6c97"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/3c9b8b8f-59f8-4dd1-a58f-2704b9d957b6_225w?s=ea212f03a5bda244f774d5429bbb6c97"
      |          }
      |        }
      |      }
      |    },
      |    {
      |      "mal_id": 441056,
      |      "url": "https://myanimelist.net/reviews.php?id=441056",
      |      "type": "anime",
      |      "reactions": {
      |        "overall": 9,
      |        "nice": 4,
      |        "love_it": 0,
      |        "funny": 5,
      |        "confusing": 0,
      |        "informative": 0,
      |        "well_written": 0,
      |        "creative": 0
      |      },
      |      "date": "2022-04-09T19:37:00+00:00",
      |      "review": "Test preliminary and spoiler review",
      |      "score": 9,
      |      "tags": [
      |        "Recommended", "Preliminary", "Spoiler"
      |      ],
      |      "is_spoiler": true,
      |      "is_preliminary": true,
      |      "episodes_watched": null,
      |      "user": {
      |        "url": "https://myanimelist.net/profile/bushman66",
      |        "username": "bushman66",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/63f75198-d1ec-4630-9ffb-acb3e376bd7d_42x62_i?s=f8b1a1342ad2943a45c51a1ad4f89ce1"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/s/common/userimages/63f75198-d1ec-4630-9ffb-acb3e376bd7d_42x62_i?s=f8b1a1342ad2943a45c51a1ad4f89ce1"
      |          }
      |        }
      |      }
      |    }
      |  ]
      |}
      |""".stripMargin)

  val testRecommendationsJson: JsValue = Json.parse(
    """
      |{
      |  "data": [
      |    {
      |      "entry": {
      |        "mal_id": 5081,
      |        "url": "https://myanimelist.net/anime/5081/Bakemonogatari",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/11/75274.jpg?s=3bb5c42c0803621dde09c52f5c4d4249",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/11/75274t.jpg?s=3bb5c42c0803621dde09c52f5c4d4249",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/11/75274l.jpg?s=3bb5c42c0803621dde09c52f5c4d4249"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/11/75274.webp?s=3bb5c42c0803621dde09c52f5c4d4249",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/11/75274t.webp?s=3bb5c42c0803621dde09c52f5c4d4249",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/11/75274l.webp?s=3bb5c42c0803621dde09c52f5c4d4249"
      |          }
      |        },
      |        "title": "Bakemonogatari"
      |      },
      |      "url": "https://myanimelist.net/recommendations/anime/5081-33263",
      |      "votes": 16
      |    },
      |    {
      |      "entry": {
      |        "mal_id": 28621,
      |        "url": "https://myanimelist.net/anime/28621/Subete_ga_F_ni_Naru",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/9/76071.jpg?s=fdc6902408ec1ded27127502ae9f0863",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/9/76071t.jpg?s=fdc6902408ec1ded27127502ae9f0863",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/9/76071l.jpg?s=fdc6902408ec1ded27127502ae9f0863"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/9/76071.webp?s=fdc6902408ec1ded27127502ae9f0863",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/9/76071t.webp?s=fdc6902408ec1ded27127502ae9f0863",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/9/76071l.webp?s=fdc6902408ec1ded27127502ae9f0863"
      |          }
      |        },
      |        "title": "Subete ga F ni Naru"
      |      },
      |      "url": "https://myanimelist.net/recommendations/anime/28621-33263",
      |      "votes": 10
      |    },
      |    {
      |      "entry": {
      |        "mal_id": 16592,
      |        "url": "https://myanimelist.net/anime/16592/Danganronpa__Kibou_no_Gakuen_to_Zetsubou_no_Koukousei_The_Animation",
      |        "images": {
      |          "jpg": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/4/51463.jpg?s=548e8ef2df2f9256802267ddc6cb07e9",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/4/51463t.jpg?s=548e8ef2df2f9256802267ddc6cb07e9",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/4/51463l.jpg?s=548e8ef2df2f9256802267ddc6cb07e9"
      |          },
      |          "webp": {
      |            "image_url": "https://cdn.myanimelist.net/images/anime/4/51463.webp?s=548e8ef2df2f9256802267ddc6cb07e9",
      |            "small_image_url": "https://cdn.myanimelist.net/images/anime/4/51463t.webp?s=548e8ef2df2f9256802267ddc6cb07e9",
      |            "large_image_url": "https://cdn.myanimelist.net/images/anime/4/51463l.webp?s=548e8ef2df2f9256802267ddc6cb07e9"
      |          }
      |        },
      |        "title": "Danganronpa: Kibou no Gakuen to Zetsubou no Koukousei The Animation"
      |      },
      |      "url": "https://myanimelist.net/recommendations/anime/16592-33263",
      |      "votes": 9
      |    }
      |  ]
      |}
      |""".stripMargin)
}