package info.fotm.armory

import info.fotm.armory.Common._
import info.fotm.armory.models.{Team, Bracket, LeaderboardRow}
import scala.util.Random

object Common {
  type Diff = (LeaderboardRow, LeaderboardRow) // previous, current
}

trait RandomExtensions {
  val seed: Int
  lazy val rng = new Random(seed)
  def randomElement[T](v: Vector[T]): T = v(rng.nextInt(v.size))
  def randomElement[T](i: Iterable[T]): T = randomElement(i.toVector)
}

trait TeamPredictor extends ((Bracket, Set[Diff]) => Set[Team])

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

object Metrics {
  def dist2(v1: Vector[Double], v2: Vector[Double]): Double =
    v1.zip(v2).map { case (l, r) => (l-r) * (l-r) }.sum

  def length2(v: Vector[Double]): Double = {
    val zero = v.map(_ => .0)
    dist2(zero, v)
  }
}