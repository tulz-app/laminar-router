package app.tulz.routing

import scala.scalajs.js

private[routing] case class RoutingState(
  path: List[String] = List.empty,
  data: Map[List[String], Any] = Map.empty
) {

  def resetPath: RoutingState = this.copy(path = List.empty)

  def enter(c: String): RoutingState = this.copy(path = c :: path)

  def getValue[T](at: List[String]): Option[T] = {
    data.get(at).map(_.asInstanceOf[T])
  }

  def setValue[T](nv: T): RoutingState = {
    val v =
      if (nv == ((): Unit) || js.isUndefined(nv)) {
        "∅"
      } else {
        nv
      }
    this.copy(data = data + (path -> v))
  }

  override def toString: String = {
    s"""${path.reverse.mkString(" ")}\n${data
      .map {
        case (key, value) => s"  ${key.reverse.mkString(" ")} -> $value"
      }.mkString("\n")}"""
  }

}
