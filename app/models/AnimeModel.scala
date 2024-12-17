package models

import java.time.Instant

case class AnimeModel(MALId: Int, title: String, titleEnglish: Option[String], `type`: String, numEpisodes: Option[Int],
                      status: String, startDate: Option[Instant], endDate: Option[Instant], maturityRating: String,
                      avgScore: Option[Double], scoredBy: Option[Int], synopsis: Option[String], genres: Seq[String], year: Option[Int])
