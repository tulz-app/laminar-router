package app.tulz.routing

private[routing] class RoutingContext {

  private var _routeChanged: Boolean                 = false
  private var dataMap: Map[DirectivePath, Any]        = Map.empty
  private var currentDataMap: Map[DirectivePath, Any] = Map.empty

  def roll(): Unit = {
    dataMap = currentDataMap
    currentDataMap = Map.empty
    _routeChanged = false
  }

  def markChanged(): Unit = {
    _routeChanged = true
  }

  def routeChanged: Boolean = _routeChanged

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
          markChanged()
        }
      case None =>
        markChanged()
    }
    put(id, nv)
  }

}
