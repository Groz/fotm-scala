package info.fotm.armory.models

sealed abstract class Gender(val id: Int) extends HasId[Int]
case object Male extends Gender(0)
case object Female extends Gender(1)

object Gender extends BuildableFromId[Int, Gender] {
  val all: Set[Gender] = Set(Male, Female)
}
