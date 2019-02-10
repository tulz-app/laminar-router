package app.tulz.routing
import scala.concurrent.Future

sealed trait RouteResult

object RouteResult {
  case class Complete(action: () => Future[() => Unit]) extends RouteResult
  case object Rejected extends RouteResult
}
