package models.relations

import play.api.libs.json.{Json, OFormat}

case class ThemeSongs(openings: Seq[String], endings: Seq[String])

object ThemeSongs {
  implicit val formats: OFormat[ThemeSongs] = Json.format[ThemeSongs]
}
