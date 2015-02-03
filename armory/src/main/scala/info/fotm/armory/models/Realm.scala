package info.fotm.armory.models

case class Realm(realmId: Int, realmName: String, realmSlug: String)

sealed abstract class Faction(val id: Int) extends HasId[Int]
case object Alliance extends Faction(0)
case object Horde extends Faction(1)

object Faction extends BuildableFromId[Int, Faction] {
  val all: Set[Faction] = Set(Alliance, Horde)
}
