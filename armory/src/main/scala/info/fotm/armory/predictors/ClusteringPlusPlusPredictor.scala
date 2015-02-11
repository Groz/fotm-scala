package info.fotm.armory.predictors

import info.fotm.armory.models._
import info.fotm.armory.{Metrics, RandomExtensions}
import scala.collection.mutable.ListBuffer

class ClusteringPlusPlusPredictor extends ClusteringPredictorBase with RandomExtensions {
  val seed = 1337

  def avg(arr: Seq[Vector[Double]]): Vector[Double] =
    arr
      .reduce((a, v) => a.zip(v).map { case (v1, v2) => v1 + v2})
      .map(_ / arr.size)

  def cluster(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team] = {
    type TeamGroup = ListBuffer[CharacterInfo]

    val size = bracket.size
    val from: ListBuffer[CharacterInfo] = rng.shuffle(matrix.keys.to[ListBuffer])

    val nClusters = matrix.size / size

    // `kmeans++`-like init
    val clusters: ListBuffer[TeamGroup] = ListBuffer[TeamGroup]()
    clusters.append(ListBuffer[CharacterInfo](from(0)))
    from.remove(0)

    for (i <- 1 until nClusters) {
      // find point furthest apart from the nearest cluster to it
      val fromNearest = for {
        charInfo <- from
        pt = matrix(charInfo)
        distances = clusters.map(_.head).map(matrix).map(v => Metrics.dist2(v, pt))
        minDist = distances.min
        clusterNum = distances.indexOf(minDist)
        ptNum = from.indexOf(charInfo)
      } yield (ptNum, clusterNum, minDist)

      val (ptNum, clusterNum, minDist) = fromNearest.maxBy(_._3)
      //println(s"New cluster: $ptNum, nearest: $clusterNum, dist: $minDist, left: ${from.size}")

      clusters.append(ListBuffer[CharacterInfo](from(ptNum)))
      from.remove(ptNum)
    }

    // fill up, for each remaining point put it into the nearest cluster to it that is not full
    var nUnfilled = nClusters

//    println(s"First: ${clusters.map(_.size)}")

    while (nUnfilled != 0) {
      // find the closest (not full cluster, point) pair and put that point into the cluster
      val all: ListBuffer[(TeamGroup, CharacterInfo, Double)] =
        for {
          cluster <- clusters if cluster.size < size
          clusterMid: Vector[Double] = {
            val result = avg(cluster.map(matrix))
            //println(result)
            result
          }
          charInfo <- from
          dist = Metrics.dist2(clusterMid, matrix(charInfo))
        } yield (cluster, charInfo, dist)

      val (cluster, point, dist) = all.minBy(_._3)

//      println( all.map { case (cluster, charInfo, dist) =>
//        (clusters.indexOf(cluster), from.indexOf(charInfo), dist)
//      } )
//
//      println(s"Chosen: ${clusters.indexOf(cluster)}, ${from.indexOf(point)}, $dist")
//      println(s"Cluster: ${cluster.map(matrix)}, $cluster\nPoint: ${matrix(point)}, $point\nDist: $dist")

      cluster.append(point)
      from.remove(from.indexOf(point))

      if (cluster.size == size)
        nUnfilled -= 1

//      println(clusters.map(_.size))

    }

    clusters.map(c => Team(bracket, c.toSet)).toSet
  }
}