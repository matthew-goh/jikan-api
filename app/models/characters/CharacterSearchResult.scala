package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterSearchResult(data: Seq[AnimeCharacter])

object CharacterSearchResult {
  implicit val formats: OFormat[CharacterSearchResult] = Json.format[CharacterSearchResult]
}
