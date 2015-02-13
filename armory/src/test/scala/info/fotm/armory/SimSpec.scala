package info.fotm.armory

import info.fotm.armory.SimApp._
import info.fotm.armory.models._
import info.fotm.armory
import org.scalatest._

class SimSpec extends FlatSpec with Matchers {

  "play" should "output 16 for equal teams" in {
    List((16, -16), (-16, 16)) should contain (play(1600, 1600))
  }

  it should "output 20 or 12 for 1500 and 1580" in {
    List((20, -20), (-12, 12)) should contain (play(1500, 1580))
  }

  it should "output 20 for 1580 and 1500" in {
    List((12, -12), (-20, 20)) should contain (play(1580, 1500))
  }

  "updateStandings" should "correctly update standings for teams" in {
    val teamA = createRandomTeam(Twos)
    val teamB = createRandomTeam(Twos)
    val stats = CharacterStats(1500, 0, 0, 0, 0)

    val standings: Map[CharacterInfo, CharacterStats] =
      { for { p <- teamA.chars ++ teamB.chars } yield (p, stats) }.toMap

    val updated = updateStandings(standings, teamA, teamB)

    updated.foreach { case (info, stats) =>
        stats.rating should not equal (1500)
        if (stats.rating > 1500) {
          stats should equal (CharacterStats(1516, 1, 0, 1, 0))
        } else {
          stats should equal (CharacterStats(1484, 0, 1, 0, 1))
        }
    }
  }

  "fScore" should "output 0 for zero p/r" in {
    fScore(0, 0, 1) should equal (0)
  }

  it should "output 1 for 100% p/r" in {
    fScore(1, 1, 1) should equal (1)
  }

  "Metrics sqrDist" should "output correct values" in {
    Metrics.dist2(Vector(0, 0), Vector(1, 0)) should equal(1)
    Metrics.dist2(Vector(0, 0), Vector(0, 1)) should equal(1)
    Metrics.dist2(Vector(1, 0), Vector(0, 1)) should equal(2)
    Metrics.dist2(Vector(1, 1, 2), Vector(-1, 3, 0)) should equal(12)
  }

}
