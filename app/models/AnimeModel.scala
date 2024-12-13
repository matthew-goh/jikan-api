package models

import java.util.Date

case class AnimeModel(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int],
                      status: String, startDate: Option[Date], endDate: Option[Date], rating: String,
                      avgScore: Option[Double], scoredBy: Option[Int], synopsis: String, genres: Seq[String], year: Int)
