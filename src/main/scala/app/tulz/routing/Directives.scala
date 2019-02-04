package app.tulz.routing

import app.tulz.routing.util.Tuple

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait Directives {

  def reject: Route = (_, _) => RouteResult.Rejected

  def extractContext: Directive1[RequestContext] =
    Directive[Tuple1[RequestContext]](
      path => inner => (ctx, rctx) => inner(Tuple1(ctx))(ctx, rctx)
    )

  def extract[T](f: RequestContext => T): Directive1[T] =
    Directive[Tuple1[T]](
      path =>
        inner =>
          (ctx, rctx) => {
            val extracted = f(ctx)
            inner(Tuple1(extracted))(ctx, rctx)
          }
    )

  def collect[T](f: PartialFunction[RequestContext, T]): Directive1[T] =
    extract(ctx => ctx).collect(f)

  def param(name: Symbol): Directive1[String] =
    paramOpt(name).collect {
      case Some(value) => value
    }

  def paramOpt(name: Symbol): Directive1[Option[String]] =
    extract(_.params.get(name.name).flatMap(_.headOption)).reportValue

  def extractUnmatchedLoc: Directive1[List[String]] =
    extract(ctx => ctx.unmatchedPath)

  def mapInnerRoute(f: Route ⇒ Route): Directive[Unit] =
    Directive(_ => inner ⇒ f(inner(())))

  def mapRequestContext(f: RequestContext ⇒ RequestContext): Directive0 =
    mapInnerRoute { inner ⇒ (ctx, rctx) ⇒
      inner(f(ctx), rctx)
    }

  def tprovide[L: Tuple](value: L): Directive[L] =
    Directive(_ => inner => inner(value))

  def pathPrefix[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    extractUnmatchedLoc.tflatMap(Some("pref")) {
      case Tuple1(unmatchedLoc) =>
        m(unmatchedLoc) match {
          case Right((t, rest)) =>
            tprovide(t)(m.tuple) & mapRequestContext(_ withUnmatchedPath rest)
          case Left(_) => reject
        }
    }.reportValue
  }

  def pathEnd: Directive0 = extractUnmatchedLoc.tflatMap(Some("end")) {
    case Tuple1(Nil) => tprovide(())
    case other       => Directive.toDirective(reject)
  }

  def path[T](m: PathMatcher[T]): Directive[T] = {
    import m.tuple
    pathPrefix(m).reportValue & pathEnd
  }

  def complete[T](action: => Future[T])(implicit ec: ExecutionContext): Route = { (_, _) =>
    RouteResult.Complete(() => action)
  }

  def complete[T](action: ToRoute)(implicit ec: ExecutionContext): Route = action.route

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
