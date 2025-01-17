package models.people

import models._
import play.api.libs.json.{Json, OFormat}

case class VoicedCharacter(role: String, anime: MediaEntry, character: BasicProfileInfo)

object VoicedCharacter {
  implicit val formats: OFormat[VoicedCharacter] = Json.format[VoicedCharacter]

  implicit class SortableVoicedCharList(val voicedCharList: Seq[VoicedCharacter]) {
    def filterByRole(status: CharacterRoles.Value): Seq[VoicedCharacter] = status match {
      case CharacterRoles.main => voicedCharList.filter(_.role == "Main")
      case CharacterRoles.supporting => voicedCharList.filter(_.role == "Supporting")
      case _ => voicedCharList
    }

    def orderBySortParameter(orderBy: VoicedCharacterOrders.Value, sortOrder: SortOrders.Value): Seq[VoicedCharacter] = {
      val sortedVoicedCharList: Seq[VoicedCharacter] = orderBy match {
        case VoicedCharacterOrders.anime => voicedCharList.sortBy(_.anime.title)
        case VoicedCharacterOrders.character => voicedCharList.sortBy(_.character.name)
        case _ => voicedCharList
      }
      if (sortOrder == SortOrders.desc) sortedVoicedCharList.reverse
      else sortedVoicedCharList
    }
  }
}
