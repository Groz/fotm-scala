package info.fotm.armory

import models._
import scala.collection.immutable.IndexedSeq
import scala.util.Random

object SimApp extends App {

  lazy val rng = new Random(1337)
  lazy val fakeRealm = Realm(10, "Fake Realm", "fakerealm")

  def randomElement[T](v: Vector[T]): T = v(rng.nextInt(v.size))
  def randomElement[T](i: Iterable[T]): T = randomElement(i.toVector)

  def createRandomCharacter: CharacterInfo = {
    val charClass = randomElement(Characters.all)
    val gender = randomElement(Gender.all)
    val race = randomElement(Race.all)
    CharacterInfo("n"+rng.nextInt().hashCode(), fakeRealm, Alliance, charClass, race, gender)
  }

  case class Team(bracket: Bracket, chars: List[CharacterInfo])

  object Team {
    def apply(bracket: Bracket, chars: Iterable[CharacterInfo]): Team =
      Team(bracket, chars.toSeq.sortBy(ci => (ci.name, ci.realm.realmSlug)).toList)
  }

  case class CharacterStats(rating: Int, wins: Int, losses: Int)

  def createRandomTeam(bracket: Bracket): Team =
    Team(bracket, (0 until bracket.size).map(_ => createRandomCharacter))

  type Standings = Map[CharacterInfo, CharacterStats]

  def change(standings: Standings, char: CharacterInfo, rating: Int): Standings = {
    val stats = standings(char)

    val newStats = CharacterStats(stats.rating + rating,
      stats.wins + (if (rating > 0) 1 else 0),
      stats.losses + (if (rating < 0) 1 else 0))

    standings.updated(char, newStats)
  }

  def calcRatingChange(winnerRating: Double, loserRating: Double): Int = {
    val chance = 1.0 / (1.0 + Math.pow(10, (loserRating - winnerRating)/400.0))
    val k = 32
    Math.round(k * (1 - chance)).toInt
  }

  def updateStandings(standings: Standings, teamA: Team, teamB: Team): Standings = {
    def teamRating(team: Team): Double = team.chars.map(standings(_).rating).sum.toDouble / team.chars.size
    val ratingChange = calcRatingChange(teamRating(teamA), teamRating(teamB))

    val afterA = teamA.chars.foldLeft(standings) { change(_, _, ratingChange) }
    val afterB = teamB.chars.foldLeft(afterA) { change(_, _, -ratingChange) }
    afterB
  }

  def history(bracket: Bracket, length: Int): Stream[(Set[Team], Leaderboard)] = {
    val maxLength = 1000
    val nGamesPerTurn = 100
    val nTeams = maxLength / bracket.size

    val allTeams: IndexedSeq[Team] = (0 until nTeams).map(_ => createRandomTeam(bracket))
    val allChars: IndexedSeq[CharacterInfo] = allTeams.flatMap(_.chars)
    val initialStandings: Standings = allChars.map { (_, CharacterStats(1500, 0, 0)) }.toMap

    def toLeaderboard(standings: Standings) = {
      val rows = standings
        .toList
        .sortBy { case (charInfo, charStats) => -charStats.rating }
        .zipWithIndex
        .map { case ((charInfo, stats), idx) =>
          LeaderboardRow(charInfo, idx+1, stats.rating, stats.wins, stats.losses, stats.wins, stats.losses) }

      Leaderboard(rows, bracket)
    }

    def play(previous: Standings): (Set[Team], Standings) = {
      val teams = rng.shuffle(allTeams).take(nGamesPerTurn * 2)

      val newStandings = teams.sliding(2, 2).foldLeft(previous) {
        (s, teams) => updateStandings(s, teams(0), teams(1))
      }

      (teams.toSet, newStandings)
    }

    def genHistory(previous: Standings, length: Int): Stream[(Set[Team], Leaderboard)] = {
      val (currentTeams, currentStandings) = play(previous)
      val currentLeaderboard = toLeaderboard(currentStandings)

      if (length == 0)
        Stream( (currentTeams, currentLeaderboard) )
      else
        (currentTeams, currentLeaderboard) #:: genHistory(currentStandings, length - 1)
    }

    genHistory(initialStandings, length)
  }

  def fScore(p: Double, r: Double, beta: Double): Double = {
    val b2 = beta * beta
    val rem = b2 * p + r
    if (rem == 0)
      0
    else
      (1 + b2) * p * r / rem
  }

  // F-score
  def evaluateStrategy(bracket: Bracket,
                       history: Stream[(Set[Team], Leaderboard)],
                      strategy: (Leaderboard, Leaderboard) => Set[Team]): Double = {

    val (guessed, predicted, total) = history.sliding(2).foldLeft(0, 0, 0) { (acc, st) =>
      val (previous, current) = (st.head, st.last)
      val actual = current._1
      val prediction = strategy(previous._2, current._2) ++ actual
      val guessed = prediction.intersect(actual)
      (acc._1 + guessed.size, acc._2 + prediction.size, acc._3 + actual.size)
    }

    val p = guessed.toDouble / predicted
    val r = guessed.toDouble / total
    fScore(p, r, 0.5)
  }

  def diff(previousLeaderboard: Leaderboard, currentLeaderboard: Leaderboard)
    : List[(LeaderboardRow, LeaderboardRow)] = {

    val previousMap: Map[CharacterInfo, LeaderboardRow] =
      previousLeaderboard.rows.map(row => (row.characterInfo, row)).toMap

    val currentMap: Map[CharacterInfo, LeaderboardRow] =
      currentLeaderboard.rows.map(row => (row.characterInfo, row)).toMap

    val changed = for {
      (charInfo, currRow) <- currentMap
      prevRow <- previousMap.get(charInfo) if prevRow.rating != currRow.rating
    } yield (prevRow, currRow)

    changed.toList
  }

  type Diff = (LeaderboardRow, LeaderboardRow) // previous, current

  def bucket(diffs: Seq[Diff]): Set[Set[Diff]] = {
    val (losers, winners) = diffs.partition { case (prev, curr) => prev.rating < curr.rating }
    val (allianceL, hordeL) = losers.partition { case (prev, curr) => curr.characterInfo.faction == Alliance }
    val (allianceW, hordeW) = winners.partition { case (prev, curr) => curr.characterInfo.faction == Alliance }
    Seq(allianceL, hordeL, allianceW, hordeW).map(_.toSet).toSet
  }

  def pickRandom(previousLeaderboard: Leaderboard, currentLeaderboard: Leaderboard): Set[Team] = {
    val diffs = diff(previousLeaderboard, currentLeaderboard)
    val buckets = bucket(diffs)
    val bracket = previousLeaderboard.bracket

    for {
      bucket: Set[Diff] <- buckets
      shuffled = rng.shuffle(bucket.toList)
      teams = shuffled.sliding(bracket.size, bracket.size)
      team: Seq[Diff] <- teams
      chars = team.map { case (prev, curr) => curr.characterInfo }
    } yield Team(bracket, chars)
  }

  def sqrDist(v1: Vector[Double], v2: Vector[Double]): Double =
    v1.zip(v2).map { case (l, r) => (l-r) * (l-r) }.sum

  def groupPopular(matrix: Map[CharacterInfo, Vector[Double]], bracket: Bracket): Set[Team] = {
    val size = bracket.size
    val entries: Set[(CharacterInfo, Vector[Double])] = rng.shuffle(matrix.toSet)

    val (_, resultTeams) = (0 until entries.size by size).foldLeft(entries, Set[Team]()) { (acc, i) =>
      val (entriesLeft, teams) = acc
      val p = randomElement(entriesLeft)
      val leftThisTurn = (0 until size-1).foldLeft(entriesLeft - p) { (left, j) =>
        val closest = left.minBy { case (charInfo, v) => sqrDist(p._2, v) }
        left - closest
      }
      val teamEntries: Set[(CharacterInfo, Vector[Double])] = entriesLeft -- leftThisTurn
      val team = Team(bracket, teamEntries.map(_._1))
      (leftThisTurn, teams + team)
    }

    resultTeams
  }

  def pickPopular(previousLeaderboard: Leaderboard, currentLeaderboard: Leaderboard): Set[Team] = {
    def metric(prevRow: LeaderboardRow, row: LeaderboardRow): Vector[Double] =
      Vector(row.rating,
        row.seasonWins, row.seasonLosses,
        row.weeklyWins, row.weeklyLosses,
        row.rating - prevRow.rating,
        (row.rating - prevRow.rating).toDouble / row.rating)

    val diffs = diff(previousLeaderboard, currentLeaderboard)
    val buckets = bucket(diffs)
    val bracket = previousLeaderboard.bracket

    for {
      bucket: Set[Diff] <- buckets if bucket.size > 0
      matrix: Map[CharacterInfo, Vector[Double]] = bucket.map { case (prev, curr) =>
        (curr.characterInfo, metric(prev, curr))
      }.toMap
      groups: Set[Team] = groupPopular(matrix, bracket)
      team <- groups
    } yield team
  }
  val h = history(Threes, 50)
  val score = evaluateStrategy(Threes, h, pickPopular)
  println(score)
}
