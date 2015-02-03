import info.fotm.armory._

object TempApp extends App {

  val armsWarrior = WarriorClass(Some(ArmsSpec))
  val anotherArmsWarrior = WarriorClass(Some(ArmsSpec))

  val otherWarrior = WarriorClass(None)

  val furyWarrior = WarriorClass(Some(FurySpec))

  println(armsWarrior)
  println(otherWarrior)
  println(armsWarrior.id)
  println(armsWarrior.spec match { case Some(specId) => specId })

  println(armsWarrior == anotherArmsWarrior)
}
