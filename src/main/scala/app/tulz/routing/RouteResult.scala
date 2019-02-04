package app.tulz.routing
import scala.concurrent.Future

sealed trait RouteResult

object RouteResult {
  case class Complete(action: () => Future[_]) extends RouteResult
  case object Rejected extends RouteResult
}
