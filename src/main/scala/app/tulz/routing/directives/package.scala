package app.tulz.routing

import app.tulz.routing.util.Tuple

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

package object directives {

  def reject: Route = _ => _ => Future.successful(RouteResult.Rejected)

  def extract[T](suffix: String)(f: RequestContext => T): Directive1[T] =
    Directive[Tuple1[T]](
      path =>
        inner =>
          ctx =>
            rctx => {
              val extracted = f(ctx)
              rctx.reportNewValue(path, extracted)
              inner(Tuple1(extracted))(ctx)(rctx)
            }
    )

  def collect[T](f: PartialFunction[RequestContext, T]): Directive1[T] =
    extract("collect")(ctx => ctx).collect(f)

  def param(name: Symbol): Directive1[String] =
    paramOpt(name).collect {
      case Some(value) => value
    }

  def paramOpt(name: Symbol): Directive1[Option[String]] =
    extract("paramOpt")(_.params.get(name.name).flatMap(_.headOption))

  def extractUnmatchedLoc: Directive1[List[String]] =
    extract("unmatchedLoc")(ctx => ctx.unmatchedPath)

  def mapInnerRoute(f: Route ⇒ Route): Directive[Unit] =
    Directive(_ => inner ⇒ f(inner(())))

  def mapRequestContext(f: RequestContext ⇒ RequestContext): Directive[Unit] =
    mapInnerRoute { inner ⇒ ctx ⇒
      inner(f(ctx))
    }

  def tprovide[L: Tuple](value: L): Directive[L] =
    Directive(_ => inner => inner(value))

  def pathPrefix[T](m: PathMatcher[T]): Directive1[T] =
    extractUnmatchedLoc.tflatMap("pathPrefix") {
      case Tuple1(unmatchedLoc) =>
        m(unmatchedLoc) match {
          case Right((t, rest)) =>
            tprovide(Tuple1(t)) & mapRequestContext(_ withUnmatchedPath rest)
          case Left(_) => reject
        }
    }

  def pathEnd: Directive0 = extractUnmatchedLoc.tflatMap("pathEnd") {
    case Tuple1(Nil) => tprovide(())
    case _           => Directive.toDirective(reject)
  }

  def path[T](m: PathMatcher[T]): Directive1[T] =
    pathPrefix(m) & pathEnd

//  def complete[T](action: ToRoute): Route = {
//    println("complete: ToRoute")
//    action.route
//  }
  def complete[T](action: Future[T])(implicit ec: ExecutionContext): Route = {
    println("complete: Future")
    _ => _ => action.map(_ => RouteResult.Complete)
  }

  def completeNow[T](action: T)(implicit ec: ExecutionContext): Route = {
    println("completeNow")
    _ => _ => Future.successful(action).map(_ => RouteResult.Complete)
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
