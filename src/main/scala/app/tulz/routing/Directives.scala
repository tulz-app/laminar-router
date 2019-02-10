package app.tulz.routing

import app.tulz.routing.util.Tuple

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Directives {

  def reject(description: String): Route = (_, rctx) => {
    rctx.rejected(description)
    RouteResult.Rejected
  }

  def extractContext: Directive1[RequestContext] =
    Directive[Tuple1[RequestContext]]("extractContext", reportValues = false)(
      inner => (ctx, rctx) => inner(Tuple1(ctx))(ctx, rctx)
    )

  def extract[T](suff: String, reportValues: Boolean)(f: RequestContext => T): Directive1[T] =
    Directive[Tuple1[T]](suff, reportValues)(
      inner =>
        (ctx, rctx) => {
          val extracted = f(ctx)
          inner(Tuple1(extracted))(ctx, rctx)
        }
    )

  def collect[T: Tuple](description: String, reportValues: Boolean)(f: PartialFunction[RequestContext, T]): Directive[T] =
    extract(description, reportValues)(ctx => ctx).collect(description)(f)

  def param(name: Symbol): Directive1[String] =
    extract(s"param(${name.name})", reportValues = true)(_.params.get(name.name).flatMap(_.headOption)).collect("param") {
      case Some(value) => Tuple1(value)
    }

  def paramOpt(name: Symbol): Directive1[Option[String]] =
    extract(s"paramOpt(${name.name})", reportValues = true)(_.params.get(name.name).flatMap(_.headOption))

  def cookie(name: String): Directive1[String] =
    extract(s"cookie($name)", reportValues = true)(_.cookies.get(name)).collect("cookie") {
      case Some(value) => Tuple1(value)
    }

  def cookieOpt(name: String): Directive1[Option[String]] =
    extract(s"cookieOpt($name)", reportValues = true)(_.cookies.get(name))

  def extractUnmatchedPath: Directive1[List[String]] =
    extract("extractUnmatchedLoc", reportValues = false)(ctx => ctx.unmatchedPath)

  def mapInnerRoute(f: Route ⇒ Route): Directive[Unit] =
    Directive("mapInnerRoute", reportValues = false)(inner ⇒ f(inner(())))

  def mapRequestContext(f: RequestContext ⇒ RequestContext): Directive0 =
    mapInnerRoute { inner ⇒ (ctx, rctx) ⇒
      inner(f(ctx), rctx)
    }

  def tprovide[L: Tuple](description: String)(value: L): Directive[L] =
    Directive(description, reportValues = false)(inner => inner(value))

  def provide[L](description: String)(value: L): Directive1[L] =
    tprovide(description)(Tuple1(value))

  def pathPrefix[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    extractUnmatchedPath
      .tflatMap(s"prefix(${m.description})", reportValues = true) {
        case Tuple1(unmatchedPath) =>
          m(unmatchedPath) match {
            case Right((t, rest)) =>
              mapRequestContext(_ withUnmatchedPath rest) & tprovide(s"prefix(${m.description}).provide")(t)(m.tuple)
            case Left(_) => reject(s"path prefix(${m.description}): no match, unmatched path: $unmatchedPath")
          }
      }
  }

  def pathEnd: Directive0 =
    extractUnmatchedPath.tflatMap("end", reportValues = true) {
      case Tuple1(Nil)   => tprovide("end.provide")(())
      case unmatchedPath => reject(s"path end: not end, unmatched path: $unmatchedPath")
    }

  def path[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    extractUnmatchedPath
      .tflatMap(s"path(${m.description})", reportValues = true) {
        case Tuple1(unmatchedPath) =>
          m(unmatchedPath) match {
            case Right((t, Nil))  => tprovide("path")(t)(m.tuple) & mapRequestContext(_ withUnmatchedPath Nil)
            case Right((_, tail)) => reject(s"path(${m.description}): not end, unmatched path: $unmatchedPath, remaining path: $tail")
            case Left(_)          => reject(s"path(${m.description}): no match, unmatched path: $unmatchedPath")
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
