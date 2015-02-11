package info.fotm.armory

import info.fotm.armory.predictors._
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

  case class TeamInfo(team: Team, gamesTogether: Int) {
    def update = TeamInfo(team, gamesTogether - 1)
    def done = gamesTogether == 0
  }

  object TeamInfo {
    def apply(team: Team): TeamInfo = TeamInfo(team, 5 + rng.nextInt(20))
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

    val allChars: Set[CharacterInfo] = (0 until 4000).map(_ => createRandomCharacter).toSet
    val nHistoryTeamsPerTurn = 1200 / bracket.size
    val nRegularTeamsPerTurn = 120 / bracket.size

    var i = 0

    def genHistory(previousTeams: Set[TeamInfo], previousStandings: Standings, nTeamsTurn: Int): Stream[(Set[Team], Standings)] = {
      // fill up currentTeams until it's equal to nTeamsPlayingPerTurn
      val takenChars: Set[CharacterInfo] = previousTeams.flatMap(_.team.chars)
      val freeChars: Vector[CharacterInfo] = rng.shuffle((allChars -- takenChars).toVector)
      val nToFill = nTeamsTurn - previousTeams.size

      val currentTeams = previousTeams ++
        freeChars
        .sliding(bracket.size, bracket.size) // TODO: implement better strategy of finding a teammate
        .take(nToFill)
        .map(chars => TeamInfo(Team(bracket, chars.toSet)))
        .toSet

      val playingTeams = rng.shuffle(currentTeams.toVector)

      // teams play each other
      val currentStandings = playingTeams.sliding(2, 2).foldLeft(previousStandings) { (standings, teams) =>
        updateStandings(standings, teams(0).team, teams(1).team)
      }

      // next iteration
      val nextTeams = currentTeams.map(_.update).filterNot(_.done)
      i += 1
      println(s"History iteration: $i")

      (currentTeams.map(_.team), currentStandings) #:: genHistory(nextTeams, currentStandings, nTeamsTurn)
    }

    val initialStandings: Standings = allChars.map { (_, CharacterStats(1500, 0, 0)) }.toMap

    println("Preparing history data...")
    val (_, currentStandings) = genHistory(Set(), initialStandings, nHistoryTeamsPerTurn).take(500).last
    println(toLeaderboard(currentStandings, bracket))
    println("Data ready.")

    genHistory(Set(), currentStandings, nRegularTeamsPerTurn).map { case (teamsPlayed, standings) =>
      (teamsPlayed, toLeaderboard(standings, bracket))
    }.take(length)
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

  //val predictor = new PopularityPredictor with VerifyingPredictor
  val predictor = new SimpleClusteringPredictor
  //val predictor = new ClusteringPlusPlusPredictor with VerifyingPredictor
  //val predictor = new ClusteringPredictor

  val score = evaluateStrategy(Threes, 100, predictor)
  println(predictor.getClass(), score)
}



