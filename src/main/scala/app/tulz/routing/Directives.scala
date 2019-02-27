package app.tulz.routing

import app.tulz.routing.util.Tuple

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Directives {

  def reject: Route = (_, rctx) => {
    RouteResult.Rejected
  }

  def extractContext: Directive1[RequestContext] =
    Directive[Tuple1[RequestContext]](
      inner => (ctx, rctx) => inner(Tuple1(ctx))(ctx, rctx)
    )

  def extract[T](f: RequestContext => T): Directive1[T] =
    Directive[Tuple1[T]](
      inner =>
        (ctx, rctx) => {
          val extracted = f(ctx)
          inner(Tuple1(extracted))(ctx, rctx)
        }
    )

  def collect[T: Tuple](f: PartialFunction[RequestContext, T]): Directive[T] =
    extract(ctx => ctx).collect(f)

  def param(name: Symbol): Directive1[String] =
    extract(_.params.get(name.name).flatMap(_.headOption)).collect {
      case Some(value) => Tuple1(value)
    }

  def paramOpt(name: Symbol): Directive1[Option[String]] =
    extract(_.params.get(name.name).flatMap(_.headOption))

  def cookie(name: String): Directive1[String] =
    extract(_.cookies.get(name)).collect {
      case Some(value) => Tuple1(value)
    }

  def cookieOpt(name: String): Directive1[Option[String]] =
    extract(_.cookies.get(name))

  def extractUnmatchedPath: Directive1[List[String]] =
    extract(ctx => ctx.unmatchedPath)

  def mapInnerRoute(f: Route ⇒ Route): Directive[Unit] =
    Directive(inner ⇒ f(inner(())))

  def mapRequestContext(f: RequestContext ⇒ RequestContext): Directive0 =
    mapInnerRoute { inner ⇒ (ctx, rctx) ⇒
      inner(f(ctx), rctx)
    }

  def tprovide[L: Tuple](value: L): Directive[L] =
    Directive(inner => inner(value))

  def provide[L](value: L): Directive1[L] =
    tprovide(Tuple1(value))

  def pathPrefix[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    extractUnmatchedPath
      .tflatMap {
        case Tuple1(unmatchedPath) =>
          m(unmatchedPath) match {
            case Right((t, rest)) =>
              mapRequestContext(_ withUnmatchedPath rest) & tprovide(t)(m.tuple)
            case Left(_) => reject
          }
      }
  }

  def pathEnd: Directive0 =
    extractUnmatchedPath.tflatMap {
      case Tuple1(Nil)   => tprovide(())
      case unmatchedPath => reject
    }

  def path[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    extractUnmatchedPath
      .tflatMap {
        case Tuple1(unmatchedPath) =>
          m(unmatchedPath) match {
            case Right((t, Nil))  => tprovide(t)(m.tuple) & mapRequestContext(_ withUnmatchedPath Nil)
            case Right((_, tail)) => reject
            case Left(_)          => reject
          }
      }
  }

  def completeF[T](action: => Future[() => Unit])(implicit ec: ExecutionContext): Route = { (_, _) =>
    RouteResult.Complete(() => action)
  }

  def complete[T](action: => Unit)(implicit ec: ExecutionContext): Route = { (_, _) =>
    RouteResult.Complete(() => Future.successful(() => action))
  }

  //  class MaybeOverrideCDirective[T, U >: T](
  //    self: Directive[T],
  //    overrideWith: PartialFunction[RequestContext, U]
  //  ) extends Directive[U]("moc") {
  //
  //    setParent(self)
  //
  //    def run(loc: Loc, ctx: RequestContext, rctx: RoutingContext): DirectiveResult[U] = {
  //      self.run(loc, ctx, rctx) match {
  //        case DirectiveResult.Missed => DirectiveResult.Missed
  //        case DirectiveResult.Matched(t, nextLoc, nextCtx) =>
  //          if (overrideWith.isDefinedAt(ctx)) {
  //            DirectiveResult.Matched(overrideWith(ctx), nextLoc, nextCtx)
  //          } else {
  //            DirectiveResult.Matched(t, nextLoc, nextCtx)
  //          }
  //      }
  //    }
  //
  //  }
  //
  //  class MaybeOverrideDirective[T, U >: T](
  //    self: Directive[T],
  //    overrideWith: RequestContext => Option[U]
  //  ) extends Directive[U]("mo") {
  //
  //    setParent(self)
  //
  //    def run(loc: Loc, ctx: RequestContext, rctx: RoutingContext): DirectiveResult[U] = {
  //      self.run(loc, ctx, rctx) match {
  //        case DirectiveResult.Missed => DirectiveResult.Missed
  //        case DirectiveResult.Matched(t, nextLoc, nextCtx) =>
  //          overrideWith(ctx) match {
  //            case None       => DirectiveResult.Matched(t, nextLoc, nextCtx)
  //            case Some(ovrd) => DirectiveResult.Matched(ovrd, nextLoc, nextCtx)
  //          }
  //      }
  //    }
  //
  //  }

}
