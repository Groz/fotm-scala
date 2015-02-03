package info.fotm.armory.models

trait HasId[ID] {
  def id: ID
}

trait BuildableFromId[ID, E <: HasId[ID]] { self =>
  def all: Set[E]
  def apply(id: ID): E = all.find(_.id == id).getOrElse(throw new NoSuchElementException(s"$self - $id"))
}
