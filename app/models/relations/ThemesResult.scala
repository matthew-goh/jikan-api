package models.relations

import play.api.libs.json.{Json, OFormat}

case class ThemesResult(data: ThemeSongs)

object ThemesResult {
  implicit val formats: OFormat[ThemesResult] = Json.format[ThemesResult]
}
