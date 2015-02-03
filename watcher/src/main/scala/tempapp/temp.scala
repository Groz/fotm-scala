package tempapp

import info.fotm.armory.models._

object TempApp extends App {

  def sameClassSpec(p1: CharacterClass[CharacterSpec], p2: CharacterClass[CharacterSpec]): Boolean =
    p1 == p2

  def sameClass(p1: CharacterClass[CharacterSpec], p2: CharacterClass[CharacterSpec]): Boolean =
    p1.id == p2.id

  var armsHealer = Characters.isHealer(ArmsWarrior)
  var mwMonk = Characters.isHealer(MistweaverMonk)
  println(s"$ArmsWarrior is a healer: $armsHealer")
  println(s"$MistweaverMonk is a healer: $mwMonk")
}
