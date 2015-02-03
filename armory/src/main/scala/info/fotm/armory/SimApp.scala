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
    Team(bracket, (0 to bracket.size).map(_ => createRandomCharacter).sortBy(ci => (ci.name, ci.realm.realmSlug)).toList)

  type Standings = Map[CharacterInfo, CharacterStats]

  def history(bracket: Bracket, length: Int): Stream[Leaderboard] = {
    val maxLength = 1000
    val nGamesPerTurn = 10
    val nTeams = maxLength / bracket.size
    val ladderLength = nTeams * bracket.size

    val allTeams: IndexedSeq[Team] = (0 to nTeams).map(_ => createRandomTeam(bracket))
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

    def play(previous: Standings): Standings = {
      val teams: IndexedSeq[Team] = rng.shuffle(allTeams).take(nGamesPerTurn * 2)
      teams.sliding(2, 2).foldLeft(previous) {
        (standings, playingTeams) => {
          
          previous.updated(playingTeams(0).chars(0), playingTeams)
        }
      }
    }

    def genHistory(previous: Standings, length: Int): Stream[Leaderboard] = {
      val currentStandings = play(previous)
      val currentLeaderboard = toLeaderboard(currentStandings)

      if (length == 0)
        Stream(currentLeaderboard)
      else
        currentLeaderboard #:: genHistory(currentStandings, length - 1)
    }

    genHistory(initialStandings, length)
  }

}
