package info.fotm.armory

import info.fotm.armory.Common._
import info.fotm.armory.models.{Team, Bracket, LeaderboardRow}
import scala.util.Random

object Common {
  type Diff = (LeaderboardRow, LeaderboardRow) // previous, current

  lazy val rng = new Random(1337)
  def randomElement[T](v: Vector[T]): T = v(rng.nextInt(v.size))
  def randomElement[T](i: Iterable[T]): T = randomElement(i.toVector)
}

trait TeamPredictor extends ((Bracket, Set[Diff]) => Set[Team]) {
}

object Metrics {
  def sqrDist(v1: Vector[Double], v2: Vector[Double]): Double =
    v1.zip(v2).map { case (l, r) => (l-r) * (l-r) }.sum
}