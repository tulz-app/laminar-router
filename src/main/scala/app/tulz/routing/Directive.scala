package app.tulz.routing
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.{ApplyConverter, Tuple}
import com.raquo.airstream.signal.{Signal, Var}

import scala.language.implicitConversions

object Directive {

  def apply[L: Tuple](f: DirectivePath => (L ⇒ Route) ⇒ Route): Directive[L] = {
    new Directive[L] {
      override def _tapply(directivePath: DirectivePath, inner: L => Route): Route = {
        f(directivePath)(inner)
      }
    }
  }

  implicit def toDirective[L: Tuple](route: Route): Directive[L] =
    Directive[L](_ => _ => route)

  implicit def addDirectiveApply[L: Tuple](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In ⇒ Route =
    f ⇒ directive.tapply("app", hac(f))

  /**
    * Adds `apply` to Directive0. Note: The `apply` parameter is call-by-name to ensure consistent execution behavior
    * with the directives producing extractions.
    */
  implicit class NullaryDirectiveExt(val directive: Directive0) extends AnyRef {

    def apply(subRoute: Route): Route = {
      directive.tapply("app", _ => subRoute)
    }

  }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R: Tuple](f: L ⇒ R): Directive[R] =
      underlying.tmap { case Tuple1(value) ⇒ f(value) }

    def collect[R](f: PartialFunction[L, R]): Directive1[R] =
      underlying.tcollect {
        case Tuple1(value) ⇒ f(value)
      }

    def flatMap[R: Tuple](f: L ⇒ Directive[R]): Directive[R] = {
      underlying.tflatMap { case Tuple1(value) ⇒ f(value) }
    }

    def filter(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter({ case Tuple1(value) ⇒ predicate(value) })
  }

}

abstract class Directive[L](implicit val ev: Tuple[L]) {
  self =>

  protected def subId(path: String, suffix: String): DirectivePath = s"$path/$suffix"

  def tapply(path: DirectivePath, inner: L ⇒ Route): Route = {
    _tapply(path, l => inner(l))
  }

  def _tapply(path: DirectivePath, inner: L ⇒ Route): Route

  def tflatMap[R: Tuple](next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R](
      path => inner ⇒ self.tapply(path, value ⇒ next(value).tapply(subId(path, "fm"), inner))
    )
  }

  def tmap[R: Tuple](f: L ⇒ R): Directive[R] =
    Directive[R](
      path => inner ⇒ self.tapply(path, value ⇒ inner(f(value)))
    )

  def &[R](next: Directive[R])(implicit composition: Composition[L, R]): Directive[composition.C] =
    self.tflatMap { l =>
      next.tmap { r =>
        composition.gc(l, r)
      }(Tuple.yes)
    }(Tuple.yes)

  def tcollect[R](f: PartialFunction[L, R]): Directive1[R] =
    self.tflatMap { t =>
      if (f.isDefinedAt(t)) {
        directives.tprovide(Tuple1(f(t)))
      } else {
        directives.reject
      }
    }

  def tfilter(predicate: L => Boolean): Directive[L] =
    self.tflatMap { t =>
      if (predicate(t)) {
        directives.tprovide(t)
      } else {
        directives.reject
      }
    }

//  def maybeOverrideC[U >: L](overrideWith: PartialFunction[RequestContext, U]): Directive[U] = new MaybeOverrideCDirective[L, U](self, overrideWith)
//
//  def maybeOverride[U >: L](overrideWith: RequestContext => Option[U]): Directive[U] = new MaybeOverrideDirective[L, U](self, overrideWith)

//  def mapTo[R](newValue: => R): Directive[R] =
//    self.tmap(_ => newValue)

  def signal: Directive1[Signal[L]] =
    self.tflatMap { t =>
      Directive[Tuple1[Signal[L]]](
        path =>
          inner =>
            (ctx, rctx) ⇒ {
              val signlPath = self.subId(path, "signal")
              rctx.get[Var[L]](signlPath) match {
                case None =>
                  val var$ = Var(t)
                  rctx.reportNewValue(signlPath, var$)
                  inner(Tuple1(var$.signal))(ctx, rctx)
                case Some(var$) =>
                  var$.writer.onNext(t)
                  inner(Tuple1(var$.signal))(ctx, rctx)
              }
            }
      )
    }

}
