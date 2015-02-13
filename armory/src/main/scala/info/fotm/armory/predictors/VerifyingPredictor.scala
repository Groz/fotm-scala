package info.fotm.armory.predictors

import info.fotm.armory.Common.Diff
import info.fotm.armory.TeamPredictor
import info.fotm.armory.models.{Bracket, Team}

trait VerifyingPredictor extends TeamPredictor {
  protected val seenThreshold = 2
  private var seen = Map[Team, Int]()

  abstract override def apply(bracket: Bracket, diffs: Set[Diff]): Set[Team] = {
    val teams = super.apply(bracket, diffs)

    seen = teams.foldLeft(seen) { case(s, t) =>
      val value = s.getOrElse(t, 0)
      s.updated(t, value + 1)
    }

    teams.filter(seen(_) >= seenThreshold)
  }
}


