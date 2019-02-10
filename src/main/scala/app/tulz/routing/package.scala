package app.tulz

import com.raquo.airstream.core.Subscription
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.Signal

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object routing {

  object directives extends Directives with PathMatchers

  def runRoute(route: Route, contexts: Signal[RequestContext])(implicit ec: ExecutionContext, owner: Owner): Subscription = {
    val routingContext = new RoutingContext

    var snapshot     = routingContext.currentDataMap
    var counter: Int = 0

    val subscription = contexts.foreach { ctx =>
      routingContext.currentDataMap = snapshot
      counter += 1
      val currentCounter = counter
      route(ctx, routingContext) match {
        case RouteResult.Complete(action) =>
          if (routingContext.routeChanged) {
            if (currentCounter == counter) {
              action().foreach { a =>
                if (currentCounter == counter) {
                  a()
                  snapshot = routingContext.currentDataMap
                }
              }
            }
          }
        case RouteResult.Rejected =>
      }

      routingContext.roll()
    }

    subscription
  }

  type Directive0      = Directive[Unit]
  type Directive1[T]   = Directive[Tuple1[T]]
  type PathMatcher0    = PathMatcher[Unit]
  type PathMatcher1[T] = PathMatcher[Tuple1[T]]

  type DirectivePath = String

  type Route = (RequestContext, RoutingContext) ⇒ RouteResult

  implicit class RouteWithConcatenation(val route: Route) {
    def ~(other: Route)(implicit ec: ExecutionContext): Route = { (ctx, rctx) ⇒
      val snapshot = rctx.currentDataMap
      route(ctx, rctx) match {
        case RouteResult.Complete(action) ⇒ RouteResult.Complete(action)
        case RouteResult.Rejected ⇒
          rctx.currentDataMap = snapshot
          other(ctx, rctx)
      }
    }
  }

}
