package models

import baseSpec.BaseSpec
import eu.timepit.refined.auto._
import play.api.data._

import java.time.Instant

class SavedAnimeSpec extends BaseSpec {
  val updateSavedAnimeForm: Form[SavedAnime] = SavedAnime.savedAnimeForm
  lazy val formData: SavedAnime = SavedAnime(33263, "Kubikiri Cycle: Aoiro Savant to Zaregotozukai", Some("The Kubikiri Cycle"),
    "OVA", Some(8), None, Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 4, None, "Closed circle mystery on an island")
  lazy val formDataInvalid: SavedAnime = SavedAnime(33263, "", Some("The Kubikiri Cycle"),
    "", Some(8), None, Some(7.75), Instant.parse("2024-12-18T10:01:49Z"), 4, None, "Closed circle mystery on an island")

  "Update saved anime form" should {
    "bind" when {
      "with a valid response" in {
        val completedForm = updateSavedAnimeForm.bind(Map(
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
        ))

        completedForm.value shouldBe Some(formData)
        completedForm.errors shouldBe List.empty
        completedForm.data shouldBe Map(
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
      }

      "with a invalid response (missing ID and title)" in {
        val completedForm = updateSavedAnimeForm.bind(Map(
          "titleEnglish" -> "The Kubikiri Cycle",
          "type" -> "OVA",
          "numEpisodes" -> "8",
          "year" -> "",
          "MALScore" -> "7.75",
          "savedAt" -> "2024-12-18T10:01:49Z",
          "episodesWatched" -> "4",
          "score" -> "",
          "notes" -> "Closed circle mystery on an island"
        ))

        completedForm.value shouldBe None
        completedForm.errors shouldBe List(
          FormError("MALId", List("error.required"), List()),
          FormError("title", List("error.required"), List())
        )
      }

      "with a invalid response (non-integer page count and invalid score)" in {
        val completedForm = updateSavedAnimeForm.bind(Map(
          "MALId" -> "33263",
          "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
          "titleEnglish" -> "The Kubikiri Cycle",
          "type" -> "OVA",
          "numEpisodes" -> "8",
          "year" -> "",
          "MALScore" -> "7.75",
          "savedAt" -> "2024-12-18T10:01:49Z",
          "episodesWatched" -> "abc",
          "score" -> "12",
          "notes" -> "Closed circle mystery on an island"
        ))

        completedForm.value shouldBe None
        completedForm.errors shouldBe List(
          FormError("episodesWatched", List("error.number"), List()),
          FormError("score", List("error.max"), List("10"))
        )
      }

      "with no response" in {
        val completedForm = updateSavedAnimeForm.bind(Map.empty[String, String])
        completedForm.value shouldBe None
        completedForm.errors shouldBe List(
          FormError("MALId", List("error.required"), List()),
          FormError("title", List("error.required"), List()),
          FormError("type", List("error.required"), List()),
          FormError("savedAt", List("error.required"), List()),
          FormError("episodesWatched", List("error.required"), List()),
          FormError("notes", List("error.required"), List())
        ) // all non-option fields of SavedAnime
      }
    }

    "fill" when {
      "with a valid response" in {
        val filledForm = updateSavedAnimeForm.fill(formData)
        filledForm.value shouldBe Some(formData)
        filledForm.errors shouldBe List.empty
        filledForm.data shouldBe Map(
          "MALId" -> "33263",
          "title" -> "Kubikiri Cycle: Aoiro Savant to Zaregotozukai",
          "titleEnglish" -> "The Kubikiri Cycle",
          "type" -> "OVA",
          "numEpisodes" -> "8",
          "MALScore" -> "7.75",
          "savedAt" -> "2024-12-18T10:01:49Z",
          "episodesWatched" -> "4",
          "notes" -> "Closed circle mystery on an island"
        ) // year and score are None, so don't appear in the Map
      }

      "with an invalid response (unsubmitted) - should not occur in practice" in {
        val filledForm = updateSavedAnimeForm.fill(formDataInvalid)
        filledForm.value shouldBe Some(formDataInvalid)
        filledForm.errors shouldBe List.empty
        filledForm.data shouldBe Map(
          "MALId" -> "33263",
          "title" -> "",
          "titleEnglish" -> "The Kubikiri Cycle",
          "type" -> "",
          "numEpisodes" -> "8",
          "MALScore" -> "7.75",
          "savedAt" -> "2024-12-18T10:01:49Z",
          "episodesWatched" -> "4",
          "notes" -> "Closed circle mystery on an island"
        )
      }
    }
  }
}
