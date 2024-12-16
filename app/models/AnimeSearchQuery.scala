package models

case class AnimeSearchQuery(search: String, minScore: Option[String] = None, maxScore: Option[String] = None,
                            orderBy: Option[String] = None, page: Int = 1)
