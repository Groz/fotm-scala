package info.fotm.armory.predictors

import info.fotm.armory.models._
import info.fotm.armory.{Metrics, RandomExtensions}

class PopularityPredictor extends ClusteringPredictorBase with RandomExtensions {
  val seed = 1337

  def cluster(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team] = {
    val size = bracket.size
    val entries: Set[(CharacterInfo, Vector[Double])] = rng.shuffle(matrix.toSet)

    def calc: Set[Team] = {
      val (_, resultTeams) = (0 until entries.size by size).foldLeft(entries, Set[Team]()) { (acc, i) =>
        val (entriesLeft, teams) = acc
        val p = randomElement(entriesLeft)
        val (leftThisTurn, currentTeam) = (0 until size-1).foldLeft(entriesLeft - p, Set(p)) { (acc, j) =>
          val (left, team) = acc
          val closest = left.minBy { case (charInfo, v) =>
            team.map(alreadyIn => Metrics.dist2(alreadyIn._2, v)).sum
          }
          (left - closest, team + closest)
        }
        val team = Team(bracket, currentTeam.map(_._1))
        (leftThisTurn, teams + team)
      }

      resultTeams
    }

    (0 until 10)
      .flatMap(_ => calc)
      .groupBy(identity).mapValues(_.size).toSeq
      .sortBy { case (team, count) => -count }
      .foldLeft(List[(Team, Int)]()) { (teams, candidate) =>
        if (teams.exists(t => t._1.chars.intersect(candidate._1.chars).size != 0))
          teams
        else
          candidate :: teams // note: reverses order
      }
      .take(entries.size / size)
      .map { case (team, count) =>
      team
    }.toSet
  }
}
