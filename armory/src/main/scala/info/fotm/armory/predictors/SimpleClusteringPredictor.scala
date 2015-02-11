package info.fotm.armory.predictors

import info.fotm.armory.models.{Bracket, CharacterInfo, Team}
import info.fotm.armory.{Metrics, RandomExtensions}

import scala.collection.mutable.ListBuffer

class SimpleClusteringPredictor extends ClusteringPredictorBase with RandomExtensions {
  val seed = 1337

  def avg(arr: Seq[Vector[Double]]): Vector[Double] =
    arr
      .reduce((a, v) => a.zip(v).map { case (v1, v2) => v1 + v2 })
      .map(_ / arr.size)

  def cluster(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team] = {
    type TeamGroup = ListBuffer[CharacterInfo]

    val size = bracket.size
    val from: ListBuffer[CharacterInfo] = rng.shuffle(matrix.keys.to[ListBuffer])

    val clusters: ListBuffer[TeamGroup] = ListBuffer[TeamGroup]()

    for (i <- 0 until matrix.size / size) {
      clusters.append(ListBuffer[CharacterInfo]())

      // create next cluster
      val cluster = clusters(i)
      cluster.append(from(0))
      from.remove(0)

      // fill it with points closest to it
      for (j <- 1 until size) {
        val mid = avg(cluster.map(matrix))
        val closest = from.minBy(charInfo => Metrics.dist2(matrix(charInfo), mid))
        cluster.append(closest)
        from.remove(from.indexOf(closest))
      }
    }

    clusters.map(c => Team(bracket, c.toSet)).toSet
  }
}
