package info.fotm.armory.predictors

import info.fotm.armory.Common._
import info.fotm.armory.TeamPredictor
import info.fotm.armory.models.{Bracket, CharacterInfo, LeaderboardRow, Team}

import scala.collection.immutable.IndexedSeq

abstract class ClusteringPredictorBase extends TeamPredictor {
  def metric(prevRow: LeaderboardRow, row: LeaderboardRow): Vector[Double] =
    Vector(//row.rating,
      row.seasonWins, row.seasonLosses,
      row.weeklyWins, row.weeklyLosses,
      row.rating - prevRow.rating,
      (row.rating - prevRow.rating).toDouble / prevRow.rating)

  protected def cluster(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team]

  override def apply(bracket: Bracket, diffs: Set[Diff]): Set[Team] = {
    val matrix: Map[CharacterInfo, Vector[Double]] = diffs.map { case (prev, curr) =>
      (curr.characterInfo, metric(prev, curr))
    }.toMap

    cluster(normalizeData(matrix), bracket)
  }

  private def normalizeData(matrix: Map[CharacterInfo, Vector[Double]]): Map[CharacterInfo, Vector[Double]] = {
    val nFeatures = matrix.head._2.size

    val ranges: IndexedSeq[(Double, Double)] = (0 until nFeatures).map { i =>
      val column = matrix.map{ case (charInfo, features) => features(i) }
      (column.min, column.max)
    }

    for {
      (charInfo, vector) <- matrix
      scaled = vector.zipWithIndex.map { case (v, i) => (v - ranges(i)._1) / ranges(i)._2 }
    } yield (charInfo, scaled)
  }

}
