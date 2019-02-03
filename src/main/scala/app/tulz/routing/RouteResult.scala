package app.tulz.routing

sealed trait RouteResult

object RouteResult {
  case object Complete extends RouteResult
  case object Rejected extends RouteResult
}
