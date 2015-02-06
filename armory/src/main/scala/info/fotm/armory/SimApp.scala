package info.fotm.armory

import models._
import info.fotm.armory.models.{CharacterInfo, Bracket}
import Common._

object SimApp extends App with RandomExtensions {
  val seed = 1337

  case class CharacterStats(rating: Int, wins: Int, losses: Int)

  lazy val fakeRealm = Realm(10, "Fake Realm", "fakerealm")

  def createRandomCharacter: CharacterInfo = {
    val charClass = randomElement(Characters.all)
    val gender = randomElement(Gender.all)
    val race = randomElement(Race.all)
    CharacterInfo("n"+rng.nextInt(), fakeRealm, Alliance, charClass, race, gender)
  }

  def createRandomTeam(bracket: Bracket): Team =
    Team(bracket, (0 until bracket.size).map(_ => createRandomCharacter).toSet)

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

  def toLeaderboard(standings: Standings, bracket: Bracket) = {
    val rows = standings
      .toList
      .sortBy { case (charInfo, charStats) => -charStats.rating }
      .zipWithIndex
      .map { case ((charInfo, stats), idx) =>
      LeaderboardRow(charInfo, idx+1, stats.rating, stats.wins, stats.losses, stats.wins, stats.losses) }

    Leaderboard(rows, bracket)
  }

  def history(bracket: Bracket, length: Int): Stream[(Set[Team], Leaderboard)] = {
    /*
    Output: stream of pairs: teams played this turn - Set[Team], leaderboard after the turn - Leaderboard
    Goals: simulate usual pattern of players playing together in teams

    - at most N players playing per each turn
    what's N equal to? average number of player updates between turns we get from version in production

    - that means N/bracket.size teams each turn

    - each team plays together for the total of rng { 3, 15 } games each burst

    - players team up with players of similar rating
     */

    val maxLength = 1000
    val nGamesPerTurn = 10
    val nTeams = maxLength / bracket.size

    val allTeams: Set[Team] = (0 until nTeams).map(_ => createRandomTeam(bracket)).toSet
    val allChars: Set[CharacterInfo] = allTeams.flatMap(_.chars)
    val initialStandings: Standings = allChars.map { (_, CharacterStats(1500, 0, 0)) }.toMap

    def play(previous: Standings): (Set[Team], Standings) = {
      val teamsToPlay = rng.shuffle(allTeams.toVector).take(nGamesPerTurn * 2)
      val substitutes: Vector[CharacterInfo] = rng.shuffle((allTeams -- teamsToPlay).flatMap(t => t.chars).toVector)
      val jumpRatio = 0.1

      var i = -1

      val teamsPlayed = for {
        t <- teamsToPlay
        players = t.chars.map(
          if (rng.nextDouble() > jumpRatio) _
          else {
            i += 1
            substitutes(i)
          })
      } yield Team(bracket, players)

      val newStandings = teamsPlayed.sliding(2, 2).foldLeft(previous) { (s, teams) =>
        updateStandings(s, teams.head, teams.last)
      }

      //println(s"Played: ${teamsPlayed.intersect(teamsToPlay).size} / ${teamsPlayed.size}")

      (teamsPlayed.toSet, newStandings)
    }

    var i = -1

    def genHistory(previous: Standings): Stream[(Set[Team], Standings)] = {
      /* log */ i += 1; println(s"History step: $i")
      val (currentTeams, currentStandings) = play(previous)
      (currentTeams, currentStandings) #:: genHistory(currentStandings)
    }

    println("Preparing history data...")
    val (_, currentStandings) = genHistory(initialStandings).take(50).last
    println("Data ready.")

    genHistory(currentStandings)
      .map { case (teams, standings) => (teams, toLeaderboard(standings, bracket)) }
      .take(length)
  }

  def fScore(p: Double, r: Double, beta: Double): Double = {
    val b2 = beta * beta
    val rem = b2 * p + r
    if (rem == 0)
      0
    else
      (1 + b2) * p * r / rem
  }

  def evaluateStrategy(bracket: Bracket,
                       history: Stream[(Set[Team], Leaderboard)],
                      predictor: TeamPredictor): Double = {

    val (guessed, predicted, total) = history.sliding(2, 1).foldLeft(0, 0, 0) { (acc, states) =>
      val ((_, prevLeaderboard), (teamsPlayed, currentLeaderboard)) = (states.head, states.last)
      val prediction = strategy(prevLeaderboard, currentLeaderboard, predictor)
      val guessed = prediction.intersect(teamsPlayed)
      (acc._1 + guessed.size, acc._2 + prediction.size, acc._3 + teamsPlayed.size)
    }

    val p = guessed.toDouble / predicted
    val r = guessed.toDouble / total
    fScore(p, r, 0.5)
  }

  def evaluateStrategy(bracket: Bracket,
                       historyLength: Int,
                       predictor: TeamPredictor): Double =
    evaluateStrategy(bracket, history(bracket, historyLength), predictor)

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

  def bucket(diffs: Seq[Diff]): Set[Set[Diff]] = {
    val (losers, winners) = diffs.partition { case (prev, curr) => prev.rating < curr.rating }
    val (allianceL, hordeL) = losers.partition { case (prev, curr) => curr.characterInfo.faction == Alliance }
    val (allianceW, hordeW) = winners.partition { case (prev, curr) => curr.characterInfo.faction == Alliance }
    Seq(allianceL, hordeL, allianceW, hordeW).map(_.toSet).toSet
  }

  def strategy(previousLeaderboard: Leaderboard,
               currentLeaderboard: Leaderboard,
               teamPredictor: TeamPredictor): Set[Team] = {
    val diffs = diff(previousLeaderboard, currentLeaderboard)
    val buckets = bucket(diffs)
    val bracket = previousLeaderboard.bracket

    for {
      bucket: Set[Diff] <- buckets if bucket.size > 0
      team <- teamPredictor(bracket, bucket)
    } yield team
  }

  val predictor = new PopularityPredictor()
//  val predictor = new RandomPredictor(rng)
  val score = evaluateStrategy(Threes, 50, predictor)
  println(score)
}



