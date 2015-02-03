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

  def history(bracket: Bracket, length: Int): Stream[(Seq[Team], Leaderboard)] = {
    val maxLength = 1000
    val nGamesPerTurn = 100
    val nTeams = maxLength / bracket.size
    val ladderLength = nTeams * bracket.size

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

    def play(previous: Standings): (Seq[Team], Standings) = {

      val teams: IndexedSeq[Team] = rng.shuffle(allTeams).take(nGamesPerTurn * 2)

      def change(standings: Standings, char: CharacterInfo, rating: Int): Standings = {
        val stats = standings(char)

        val newStats = CharacterStats(stats.rating + rating,
          stats.wins + (if (rating > 0) 1 else 0),
          stats.losses + (if (rating < 0) 1 else 0))

        standings.updated(char, newStats)
      }

      val newStandings = teams.sliding(2, 2).foldLeft(previous) {
        (standings, playingTeams) => {
          val (teamA, teamB) = (playingTeams(0), playingTeams(1))

          def teamRating(team: Team): Double = team.chars.map(standings(_).rating).sum.toDouble / bracket.size
          val (ratingA, ratingB) = (teamRating(teamA), teamRating(teamB))
          def chance(left: Double, right: Double) = 1.0 / (1.0 + Math.pow(10, (right - left)/400.0))
          val (chanceA, chanceB) = (chance(ratingA, ratingB), chance(ratingB, ratingA))
          val k = 32
          val (ratingChangeA, ratingChangeB) = (k * (1 - chanceA), k * (1 - chanceB))

          val afterA = teamA.chars.foldLeft(standings) { change(_, _, ratingChangeA.toInt) }
          val afterB = teamB.chars.foldLeft(afterA) { change(_, _, -ratingChangeB.toInt) }
          afterB
        }
      }

      (teams, newStandings)
    }

    def genHistory(previous: Standings, length: Int): Stream[(Seq[Team], Leaderboard)] = {
      val (currentTeams, currentStandings) = play(previous)
      val currentLeaderboard = toLeaderboard(currentStandings)

      if (length == 0)
        Stream( (currentTeams, currentLeaderboard) )
      else
        (currentTeams, currentLeaderboard) #:: genHistory(currentStandings, length - 1)
    }

    genHistory(initialStandings, length)
  }

  val h = history(Threes, 100)
  println(h.last._2)
}
