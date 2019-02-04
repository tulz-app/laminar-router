package app.tulz.routing

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
//    println(s"enter: $name")
//    println(s"  ${_fullPathStr}")
  }

  def leave(): Unit = {
    _fullPath = _fullPath.tail
    _fullPathStr = pathToStr(_fullPath)
//    println(s"leave")
//    println(s"  ${_fullPathStr}")
  }

  def roll(): Unit = {
    previousDataMap = currentDataMap
    currentDataMap = Map.empty
//    println("rolled")
  }

  def routeChanged: Boolean = {
//    println(s"route changed: ${previousDataMap} != ${currentDataMap}")
    previousDataMap != currentDataMap
  }

  def previousValue[T]: Option[T] = {
//    println(s"    getting previous value for ${_fullPathStr} -> ${previousDataMap.get(_fullPathStr)}")
    previousDataMap.get(_fullPathStr).map(_.asInstanceOf[T])
  }

  def reportNewValue[T](nv: T): Unit = {
//    println(s"  new value reported for ${_fullPathStr}")
    previousDataMap.get(_fullPathStr) match {
      case Some(previousValue) =>
        val pv = previousValue.asInstanceOf[T]
        if (pv != nv) {
//          println(s"    value changed    : $nv (old - $pv)")
        } else {
//          println(s"    value not changed: $nv")
        }
      case None =>
//        println(s"      new value        : $nv")
    }
    currentDataMap = currentDataMap + (_fullPathStr -> nv)
  }

}
