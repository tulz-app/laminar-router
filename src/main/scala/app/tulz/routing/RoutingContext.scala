package app.tulz.routing

import scala.scalajs.js

private[routing] class RoutingContext {

  private var previousDataMap: Map[String, Any]         = Map.empty
  private[routing] var currentDataMap: Map[String, Any] = Map.empty

  private var _fullPathStr: String = ""

  def enter(c: Char): Unit = {
    _fullPathStr = _fullPathStr + c
  }

  def leave(): Unit = {
    _fullPathStr = _fullPathStr.dropRight(1)
  }

  def roll(): Unit = {
    previousDataMap = currentDataMap
    currentDataMap = Map.empty
  }

  def routeChanged: Boolean = {
    previousDataMap != currentDataMap
  }

  def previousValue[T]: Option[T] = {
    previousDataMap.get(_fullPathStr).map(_.asInstanceOf[T])
  }

  def reportNewValue[T](nv: T): Unit = {
    if (nv != () && !js.isUndefined(nv)) {
      currentDataMap = currentDataMap + (_fullPathStr -> nv)
    } else {
      currentDataMap = currentDataMap - _fullPathStr
    }
  }

}
