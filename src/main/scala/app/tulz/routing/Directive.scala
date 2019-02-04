package app.tulz.routing
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.{ApplyConverter, Tuple}
import com.raquo.airstream.signal.{Signal, Var}

import scala.language.implicitConversions
import scala.scalajs.js

object Directive {

  def apply[L: Tuple](path: DirectivePath, reportValues: Boolean)(f: (L ⇒ Route) ⇒ Route): Directive[L] = {
    new Directive[L](path, reportValues, inner => f(inner))
  }

  implicit def toDirective[L: Tuple](route: Route): Directive[L] =
    Directive[L]("toDirective", reportValues = false)(_ => route)

  implicit def addDirectiveApply[L: Tuple](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In ⇒ Route =
    f ⇒ directive.tapply(hac(f))

  implicit class NullaryDirectiveExt(val directive: Directive0) extends AnyRef {

    def apply(subRoute: Route): Route = {
      directive.tapply(_ => subRoute)
    }

  }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R: Tuple](f: L ⇒ R): Directive[R] =
      underlying.tmap { case Tuple1(value) ⇒ f(value) }

    def collect[R: Tuple](description: String)(f: PartialFunction[L, R]): Directive[R] =
      underlying.tcollect(description) {
        case Tuple1(value) ⇒ f(value)
      }

//    def flatMap[R: Tuple](suff: Option[String])(f: L ⇒ Directive[R]): Directive[R] = {
//      underlying.tflatMap(suff) { case Tuple1(value) ⇒ f(value) }
//    }

    def filter(description: String)(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter(description)({ case Tuple1(value) ⇒ predicate(value) })

    def signal: Directive1[Signal[L]] =
      Directive[Tuple1[Signal[L]]](s"${underlying.path}.signal", reportValues = false) { inner => (ctx, rctx) =>
        underlying.tapply { value ⇒
          rctx.previousValue[Var[L]] match {
            case None =>
              val var$ = Var(value._1)
//              println(s"signal initial value: ${value._1}")
              // TODO don't understand this stuff, but this directive never 'enters' before this code, so the value is saved for the underlying directive, which is what we want
              rctx.reportNewValue(var$)
              inner(Tuple1(var$.signal))
            case Some(var$) =>
              rctx.reportNewValue(var$)
              var$.writer.onNext(value._1)
//              println(s"signal new value: ${value._1}")
              inner(Tuple1(var$.signal))
          }
        }(ctx, rctx)
      }

  }

}

class Directive[L](
  val path: DirectivePath,
  val reportValues: Boolean,
  val _tapply: (L ⇒ Route) => Route
)(implicit val ev: Tuple[L]) {
  self =>

  def tapply(inner: L ⇒ Route): Route = {
    _tapply(
      value =>
        (ctx, rctx) => {
          rctx.enter(path)
          if (reportValues) {
            if (value == ()) {
              rctx.reportNewValue(true)
            } else {
              rctx.reportNewValue(value)
            }
          }
          val result = inner(value)(ctx, rctx)
          rctx.leave()
          result
        }
    )
  }

  def tflatMap[R: Tuple](path: String, reportValues: Boolean)(next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R](path, reportValues) { inner ⇒
      self._tapply(value ⇒ next(value)._tapply(inner))
    }
  }

  def tmap[R: Tuple](f: L ⇒ R): Directive[R] =
    Directive[R]("tmap", self.reportValues)(
      inner ⇒ self._tapply(value ⇒ inner(f(value)))
    )

  def &[R](next: Directive[R])(implicit composition: Composition[L, R]): Directive[composition.C] =
    Directive[composition.C](s"${self.path}&${next.path}", self.reportValues || next.reportValues) { inner =>
      self._tapply { l ⇒
        next._tapply { r => (ctx, rctx) =>
          inner(composition.gc(l, r))(ctx, rctx)
        }
      }
    }(Tuple.yes)

  def tcollect[R: Tuple](description: String)(f: PartialFunction[L, R]): Directive[R] =
    Directive[R](s"${self.path}.${description}", self.reportValues) { inner ⇒
      self._tapply(
        value ⇒
          if (f.isDefinedAt(value)) {
            inner(f(value))
          } else {
            directives.reject
          }
      )
    }

  def tfilter(description: String)(predicate: L => Boolean): Directive[L] =
    Directive[L](s"${self.path}.${description}", self.reportValues) { inner ⇒
      self._tapply(
        value ⇒
          if (predicate(value)) {
            inner(value)
          } else {
            directives.reject
          }
      )
    }

//  def maybeOverrideC[U >: L](overrideWith: PartialFunction[RequestContext, U]): Directive[U] = new MaybeOverrideCDirective[L, U](self, overrideWith)
//
//  def maybeOverride[U >: L](overrideWith: RequestContext => Option[U]): Directive[U] = new MaybeOverrideDirective[L, U](self, overrideWith)

//  def mapTo[R](newValue: => R): Directive[R] =
//    self.tmap(_ => newValue)

}
