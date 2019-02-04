package app.tulz

import app.tulz.routing.util.ApplyConverterInstances
import com.raquo.airstream.core.Subscription
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.Signal

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object routing {

  object directives extends Directives with PathMatchers

  def runRoute(route: Route, contexts: Signal[RequestContext])(implicit ec: ExecutionContext, owner: Owner): Subscription = {
    val routingContext = new RoutingContext

    val subscription = contexts.foreach { ctx =>
      route(ctx, routingContext) match {
        case RouteResult.Rejected => ()
        case RouteResult.Complete(action) =>
          if (routingContext.routeChanged) {
            action() // TODO future here unused
          } else {
            ()
          }
      }

      routingContext.roll()
    }

    subscription
  }


  type Directive0 = Directive[Unit]
  type Directive1[T] = Directive[Tuple1[T]]
  type PathMatcher0 = PathMatcher[Unit]
  type PathMatcher1[T] = PathMatcher[Tuple1[T]]

  type DirectivePath = String

  type Route = (RequestContext, RoutingContext) ⇒ RouteResult

  implicit class RouteWithConcatenation(val route: Route) {
    def ~(other: Route)(implicit ec: ExecutionContext): Route = { (ctx, rctx) ⇒
      val snapshot = rctx.currentDataMap
      println(s"saved snapshot: $snapshot")
      route(ctx, rctx) match {
        case RouteResult.Complete(action) ⇒ RouteResult.Complete(action)
        case RouteResult.Rejected ⇒
          println(s"reverting to snapshot: ${rctx.currentDataMap} -> $snapshot")
          rctx.currentDataMap = snapshot
          other(ctx, rctx)
      }
    }
  }

  trait ToRoute {

    def route: Route

  }

  implicit def anyToRoute[T](u: => T)(implicit ec: ExecutionContext): ToRoute = {
    new ToRoute {
      override def route: Route =
        (_, _) => RouteResult.Complete(() => Future.successful(u))
    }
  }

}
