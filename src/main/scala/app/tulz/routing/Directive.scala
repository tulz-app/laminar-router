package app.tulz.routing
import app.tulz.debug.Logging
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.{ApplyConverter, Tuple}
import com.raquo.airstream.signal.{Signal, Var}

import scala.language.implicitConversions

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

    def filter(description: String)(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter(description)({ case Tuple1(value) ⇒ predicate(value) })

    def signal: Directive1[Signal[L]] =
      Directive[Tuple1[Signal[L]]](s"${underlying.path}.signal", reportValues = false) { inner => (ctx, rctx) =>
//        underlying.copy(reportValues = false).tapply { value ⇒
      Logging.trace(s"signal - calling underlying.tapply")
        underlying.tapply { value ⇒ // TODO figure this out, when this is run, enter is not yet called
          Logging.trace(s"signal - underlying.tapply: $value")
          rctx.previousValue[Var[L]] match {
            case None =>
              val var$ = Var(value._1)
//              Logging.trace(s"signal initial value: ${value._1}")
              // TODO don't understand this stuff, but this directive never 'enters' before this code, so the value is saved for the underlying directive, which is what we want
              rctx.reportNewValue(var$)
              Logging.trace(s"signal - calling inner")
              inner(Tuple1(var$.signal))
            case Some(var$) =>
              rctx.reportNewValue(var$)
              var$.writer.onNext(value._1)
//              Logging.trace(s"signal new value: ${value._1}")
              Logging.trace(s"signal - calling inner")
              inner(Tuple1(var$.signal))
          }
        }(ctx, rctx)
      }

  }

}

trait ConjunctionMagnet[L] {
  type Out
  def apply(underlying: Directive[L]): Out
}

object ConjunctionMagnet {
  implicit def fromDirective[L, R](other: Directive[R])(implicit composition: Composition[L, R]): ConjunctionMagnet[L] { type Out = Directive[composition.C] } =
    new ConjunctionMagnet[L] {
      type Out = Directive[composition.C]
      def apply(underlying: Directive[L]) =
        Directive[composition.C](s"${underlying.path}&${other.path}", underlying.reportValues || other.reportValues) {
          inner ⇒
            underlying.tapply { prefix ⇒ other.tapply { suffix ⇒ inner(composition.gc(prefix, suffix)) } }
        }(Tuple.yes) // we know that join will only ever produce tuples
    }
}

class Directive[L](
  val path: DirectivePath,
  val reportValues: Boolean,
  private val _tapply: (L ⇒ Route) => Route
)(implicit val ev: Tuple[L]) {
  self =>

  def copy(
    path: DirectivePath = this.path,
    reportValues: Boolean = this.reportValues
  ): Directive[L] = new Directive[L](path, reportValues, this._tapply)

  def tapply(inner: L ⇒ Route): Route = {
    _tapply(
      value =>
        (ctx, rctx) => {
          if (reportValues) {
            rctx.enter(path)
            if (value == ()) {
              rctx.reportNewValue(true)
            } else {
              rctx.reportNewValue(value)
            }
          }
          if (reportValues) {
            Logging.trace("calling inner...")
          }
          val result = inner(value)(ctx, rctx)
          if (reportValues) {
            rctx.leave()
          }
          result
        }
    )
  }

  def tflatMap[R: Tuple](path: String, reportValues: Boolean)(next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R](path, reportValues) { inner ⇒
      self.tapply(value ⇒ next(value).tapply(inner))
    }
  }

  def tmap[R: Tuple](f: L ⇒ R): Directive[R] =
    Directive[R]("tmap", self.reportValues)(
      inner ⇒ self._tapply(value ⇒ inner(f(value)))
    )

  def &[R](magnet: ConjunctionMagnet[L]): magnet.Out = magnet(this)

  def tcollect[R: Tuple](description: String)(f: PartialFunction[L, R]): Directive[R] =
    Directive[R](s"${self.path}.${description}", self.reportValues) { inner ⇒
      self.tapply(
        value ⇒
          if (f.isDefinedAt(value)) {
            inner(f(value))
          } else {
            directives.reject(s"collect: $description")
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
            directives.reject(s"filter:  $description")
          }
      )
    }

}
