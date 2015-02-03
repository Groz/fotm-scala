package info.fotm.armory.models

case class CharacterInfo(
  name: String,
  realm: Realm,
  faction: Faction,
  spec: CharacterClass[CharacterSpec],
  race: Race,
  gender: Gender)

case class LeaderboardRow(
  characterInfo: CharacterInfo,
  ranking: Int,
  rating: Int,
  seasonWins: Int,
  seasonLosses: Int,
  weeklyWins: Int,
  weeklyLosses: Int) {
  val seasonTotal = seasonWins + seasonLosses
  val weeklyTotal = weeklyWins + weeklyLosses
}