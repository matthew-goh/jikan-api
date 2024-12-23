package models.characters

import play.api.libs.json.{Json, OFormat}

case class CharacterImages(jpg: JpgImage)

object CharacterImages {
  implicit val formats: OFormat[CharacterImages] = Json.format[CharacterImages]
}
