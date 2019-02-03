package app.tulz

import com.raquo.airstream.core.Subscription
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.Signal

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object routing {

  def runRoute(route: Route, contexts: Signal[RequestContext])(implicit owner: Owner): Subscription = {
    val routingContext = new RoutingContext

    val subscription = contexts.foreach { ctx =>
      if (routingContext.routeChanged) {
        route(ctx)(routingContext)
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

  type Route = RequestContext => RoutingContext ⇒ Future[RouteResult]

  implicit class RouteWithConcatenation(val route: Route) {
    def ~(other: Route)(implicit ec: ExecutionContext): Route = { ctx => rctx ⇒
      route(ctx)(rctx).flatMap {
        case RouteResult.Complete ⇒ Future.successful(RouteResult.Complete)
        case RouteResult.Rejected ⇒ other(ctx)(rctx)
      }
    }
  }

  trait ToRoute {

    def route: Route

  }

  implicit def anyToRoute[T](u: => T)(implicit ec: ExecutionContext): ToRoute = {
    println("anyToRoute")
    new ToRoute {
      override def route: Route =
        _ => _ => Future.successful(u).map(_ => RouteResult.Complete)

    }
  }

}
