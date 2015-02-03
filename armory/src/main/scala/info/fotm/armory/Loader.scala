package info.fotm.armory

import info.fotm.armory.models._
import dispatch._, Defaults._
import org.json4s.{DefaultFormats, JValue}

case class LeaderboardEntryRaw(
  ranking: Int,
  rating: Int,
  name: String,
  realmId: Int,
  realmName: String,
  realmSlug: String,
  raceId: Int,
  classId: Int,
  specId: Int,
  factionId: Int,
  genderId: Int,
  seasonWins: Int,
  seasonLosses: Int,
  weeklyWins: Int,
  weeklyLosses: Int)

case class RawArmory(rows: List[LeaderboardEntryRaw])

class Loader[L <: Locale](apiKey: String, region: Region[L], localeOption: Option[L] = None) {

  val locale: L = localeOption.getOrElse(region.locales.head)

  private val apiRoot: Req = host(s"${region.name.toLowerCase}.api.battle.net").secure / "wow"

  private def buildUrl(route: String*): Req =
    route.foldLeft(apiRoot) (_ / _) <<? Map("locale" -> locale.name, "apikey" -> apiKey)

  private def get(r: Req): Future[JValue] = Http(r.GET OK as.json4s.Json)

  def leaderboardsUrl(bracket: Bracket): Req = buildUrl("leaderboard", bracket.url)

  private def leaderboardsRaw: Bracket => Future[JValue] = leaderboardsUrl _ andThen get

  implicit val formats = DefaultFormats

  private def toEntry(raw: LeaderboardEntryRaw, rank: Int): LeaderboardRow = {
    val realm = Realm(raw.realmId, raw.realmName, raw.realmSlug)

    val specId = if (raw.specId == 0) None else Some(raw.specId)

    val charInfo = CharacterInfo(raw.name,
      realm,
      Faction(raw.factionId),
      Characters.create(raw.classId, specId),
      Race(raw.raceId),
      Gender(raw.genderId))

    LeaderboardRow(charInfo, rank+1, raw.rating, raw.seasonWins, raw.seasonLosses, raw.weeklyWins, raw.weeklyLosses)
  }

  def leaderboards(bracket: Bracket): Future[Leaderboard] =
    leaderboardsRaw(bracket).map { jsonResponse =>
      println("Response received.")
      val rows = jsonResponse.extract[RawArmory]
        .rows
        .sortBy(r => (-r.rating, r.name, r.realmId)) // avoid considering shuffled duplicates as different retrievals
        .zipWithIndex.map { case (raw, idx) => toEntry(raw, idx) }
      Leaderboard(rows, bracket)
    }
}