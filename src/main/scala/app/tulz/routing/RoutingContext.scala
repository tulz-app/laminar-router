package app.tulz.routing

import scala.scalajs.js

private[routing] class RoutingContext {

  private var previousDataMap: Map[List[String], Any] = Map.empty
  private var _currentDataMap: Map[List[String], Any] = Map.empty
  private var _fullPathStr: List[String] = List("^")

  val currentDataMap: Map[List[String], Any] = _currentDataMap

  def enter(c: String): Unit = {
    val pr = _fullPathStr
    _fullPathStr = c :: _fullPathStr
  }

  def leave(): Unit = {
    val pr = _fullPathStr
    _fullPathStr = _fullPathStr.tail
  }

  def setDataMap(m: Map[List[String], Any]): Unit = {
    _currentDataMap = m
  }

  def roll(): Unit = {
    previousDataMap = _currentDataMap
    _currentDataMap = Map.empty
  }

  def routeChanged: Boolean = {
    previousDataMap != _currentDataMap
  }

  def previousValue[T]: Option[T] = {
    previousDataMap.get(_fullPathStr).map(_.asInstanceOf[T])
  }

  def reportNewValue[T](nv: T): Unit = {
    val v =
      if (nv == ((): Unit) || js.isUndefined(nv)) {
        "∅"
      } else {
        nv
      }
    _currentDataMap = _currentDataMap + (_fullPathStr -> v)
  }

}
