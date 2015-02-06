package info.fotm.armory

import info.fotm.armory.models._
import Common._

class PopularityPredictor extends TeamPredictor {
  override def apply(bracket: Bracket, diffs: Set[Diff]): Set[Team] = {
    val matrix: Map[CharacterInfo, Vector[Double]] = diffs.map { case (prev, curr) =>
      (curr.characterInfo, metric(prev, curr))
    }.toMap
    groupPopular(matrix, bracket)
  }

  def metric(prevRow: LeaderboardRow, row: LeaderboardRow): Vector[Double] =
    Vector(//row.rating,
      row.seasonWins, row.seasonLosses,
      row.weeklyWins, row.weeklyLosses,
      row.rating - prevRow.rating,
      (row.rating - prevRow.rating).toDouble / prevRow.rating)

  def groupPopular(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team] = {
    val size = bracket.size
    val entries: Set[(CharacterInfo, Vector[Double])] = rng.shuffle(matrix.toSet)

    def calc: Set[Team] = {
      val (_, resultTeams) = (0 until entries.size by size).foldLeft(entries, Set[Team]()) { (acc, i) =>
        val (entriesLeft, teams) = acc
        val p = randomElement(entriesLeft)
        val (leftThisTurn, teamEntries) = (0 until size-1).foldLeft(entriesLeft - p, Set(p)) { (acc, j) =>
          val (left, team) = acc
          val closest = left.minBy { case (charInfo, v) =>
            team.map(alreadyIn => Metrics.sqrDist(alreadyIn._2, v)).sum
          }
          (left - closest, team + closest)
        }
        val team = Team(bracket, teamEntries.map(_._1))
        (leftThisTurn, teams + team)
      }

      resultTeams
    }

    println("Bucket:")

    (0 until 30)
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
      //println(s"Team popularity: $count")
      team
    }.toSet
  }
}
