package info.fotm.armory

import models._
import scala.collection.immutable.IndexedSeq
import scala.util.Random

object SimApp extends App {

  val rng = new Random(1337)
  val fakeRealm = Realm(10, "Fake Realm", "fakerealm")

  def randomElement[T](set: Set[T]): T = set.toVector(rng.nextInt(set.size))

  def createRandomCharacter: CharacterInfo = {
    val charClass = randomElement(Characters.all)
    val gender = randomElement(Gender.all)
    val race = randomElement(Race.all)
    CharacterInfo("n"+rng.nextInt().hashCode(), fakeRealm, Alliance, charClass, race, gender)
  }

  case class Team(bracket: Bracket, chars: List[CharacterInfo])
  case class CharacterStats(rating: Int, wins: Int, losses: Int)

  def createRandomTeam(bracket: Bracket): Team =
    Team(bracket, (0 until bracket.size).map(_ => createRandomCharacter).sortBy(ci => (ci.name, ci.realm.realmSlug)).toList)

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

  def updateStandings(standings: Standings, playingTeams: Seq[Team]): Standings = {
    val (teamA, teamB) = (playingTeams(0), playingTeams(1))

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
      val newStandings = teams.sliding(2, 2).foldLeft(previous) { updateStandings }
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

  def evaluateStrategy(bracket: Bracket,
                       history: Stream[(Set[Team], Leaderboard)],
                      strategy: (Leaderboard, Leaderboard) => Set[Team]) = {

    history.sliding(2).foldLeft(0, 0) { (acc, st) =>
      val (previous, current) = (st.head, st.last)
      val prediction = strategy(previous._2, current._2)
      val actual = current._1
      val guessed = prediction.intersect(actual)
      (acc._1 + guessed.size, acc._2 + actual.size)
    }

  }

  def pickRandom(l1: Leaderboard, l2: Leaderboard): Set[Team] = {
    val m1: Map[CharacterInfo, LeaderboardRow] = l1.rows.map(row => (row.characterInfo, row)).toMap
    val m2: Map[CharacterInfo, LeaderboardRow] = l2.rows.map(row => (row.characterInfo, row)).toMap

    Set()
  }

  val h = history(Threes, 100)
  val (guessed, total) = evaluateStrategy(Threes, h, pickRandom)
  println(guessed, total)
}
