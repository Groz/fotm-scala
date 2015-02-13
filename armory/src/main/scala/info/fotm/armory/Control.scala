package info.fotm.armory

/**
 * Created by tmagomedov on 2/12/15.
 */
object Control {

  def using[A <: { def close(): Unit }, B] (param: A) (f: A => B): B =
    try {
      f(param)
    } finally {
      param.close()
    }
}
