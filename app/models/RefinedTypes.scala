package models

import eu.timepit.refined.api._
import eu.timepit.refined.numeric._

object RefinedTypes {
  type NaturalNum = Int Refined NonNegative
  type ScoreInt = Int Refined Interval.Closed[1, 10]
  type Percentage = Double Refined Interval.Closed[0, 100]
}
