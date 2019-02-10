package app.tulz.routing

import app.tulz.debug.Logging

private[routing] class RoutingContext {

  private var previousDataMap: Map[DirectivePath, Any]        = Map.empty
  private[routing] var currentDataMap: Map[DirectivePath, Any] = Map.empty

  private var _fullPath: List[String] = Nil
  private var _fullPathStr: String = ""

  private def pathToStr(p: List[String]): String =
    p.reverse.mkString(" / ")

  def enter(name: String): Unit = {
    _fullPath = name :: _fullPath
    _fullPathStr = pathToStr(_fullPath)
    Logging.debug(s"enter: $name", _fullPathStr)
  }

  def leave(): Unit = {
    _fullPath = _fullPath.tail
    _fullPathStr = pathToStr(_fullPath)
    Logging.debug(s"leave", _fullPathStr)
  }

  def rejected(description: String): Unit = {
    Logging.debug(s"rejected: $description")
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
    Logging.debug(s"new value for ${_fullPathStr}", nv)
    currentDataMap = currentDataMap + (_fullPathStr -> nv)
  }

}
