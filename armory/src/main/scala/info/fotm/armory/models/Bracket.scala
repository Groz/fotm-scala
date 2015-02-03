package info.fotm.armory.models

sealed abstract class Bracket(val size: Int) {
  val url = size + "v" + size
}

case object Twos extends Bracket(2)
case object Threes extends Bracket(3)
case object Fives extends Bracket(5)

case object Rbg extends Bracket(10) {
  override val url = "rbg"
}

object Bracket {
  val all = Set(Twos, Threes, Fives, Rbg)
}