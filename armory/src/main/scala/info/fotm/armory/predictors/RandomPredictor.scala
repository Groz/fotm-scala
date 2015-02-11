package info.fotm.armory.predictors

import info.fotm.armory.Common.Diff
import info.fotm.armory.models.{Bracket, Team}
import info.fotm.armory.{RandomExtensions, TeamPredictor}

class RandomPredictor extends TeamPredictor with RandomExtensions {
  val seed = 1337

  override def apply(bracket: Bracket, bucket: Set[Diff]): Set[Team] = {
    val shuffled = rng.shuffle(bucket.toList)
    val teams = shuffled.sliding(bracket.size, bracket.size)
    (for {
      team: Seq[Diff] <- teams
      chars = team.map { case (prev, curr) => curr.characterInfo }
    } yield Team(bracket, chars.toSet)).toSet
  }
}
