package app.tulz.routing

private[routing] class RoutingContext {

  private[routing] var _routeChanged: Set[String]              = Set.empty
  private var dataMap: Map[DirectivePath, Any]        = Map.empty
  private var currentDataMap: Map[DirectivePath, Any] = Map.empty

  def roll(): Unit = {
    dataMap = currentDataMap
    currentDataMap = Map.empty
    _routeChanged = Set.empty
  }

  def markChanged(id: DirectivePath): Unit = {
    println(s"changed: $id")
    _routeChanged = _routeChanged + id
  }

  def undoChanged(id: DirectivePath): Unit = {
    println(s"undo changed: $id")
    _routeChanged = _routeChanged - id
  }

  def routeChanged: Boolean = _routeChanged.nonEmpty

  def put[T](id: DirectivePath, data: T): Unit = {
    currentDataMap = currentDataMap + (id -> data)
  }

  def get[T](id: DirectivePath): Option[T] = {
    dataMap.get(id).map(_.asInstanceOf[T])
  }

  def reportNewValue[T](id: DirectivePath, nv: T): Unit = {
    dataMap.get(id) match {
      case Some(previousValue) =>
        val pv = previousValue.asInstanceOf[T]
        if (pv != nv) {
          println(s"new value")
          println(s"  $id: $pv -> $nv")
          markChanged(id)
        } else {
          println(s"same value")
          println(s"  $id: $pv -> $nv")
        }
      case None =>
        println(s"new value")
        println(s"  $id: _ -> $nv")
        markChanged(id)
    }
    put(id, nv)
  }

}
