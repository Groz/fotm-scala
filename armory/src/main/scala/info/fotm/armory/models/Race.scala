package info.fotm.armory.models

sealed abstract class Race(val id: Int) extends HasId[Int]

case object Human extends Race(1)
case object Orc extends Race(2)
case object Dwarf extends Race(3)
case object NightElf extends Race(4)
case object Undead extends Race(5)
case object Tauren extends Race(6)
case object Gnome extends Race(7)
case object Troll extends Race(8)
case object Goblin extends Race(9)
case object BloodElf extends Race(10)
case object Draenei extends Race(11)
case object Worgen extends Race(22)
case object PandarenNeutral extends Race(24)
case object PandarenAlliance extends Race(25)
case object PandarenHorde extends Race(26)

object Race extends BuildableFromId[Int, Race] {
  val horde: Set[Race] = Set(Orc, Troll, Tauren, Undead, Goblin, BloodElf, PandarenHorde)
  val alliance: Set[Race] = Set(Human, NightElf, Dwarf, Gnome, Draenei, Worgen, PandarenAlliance)
  val all: Set[Race] = horde ++ alliance + PandarenNeutral
}