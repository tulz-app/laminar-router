package app.tulz.routing

import app.tulz.routing.util.ApplyConverter
import app.tulz.util.Tuple
import com.raquo.airstream.eventstream.EventStream
import com.raquo.airstream.signal.Signal
import com.raquo.airstream.signal.Var

import scala.language.implicitConversions

class Directive[L](
  val tapply: (L => Route) => Route
)(implicit val ev: Tuple[L]) {
  self =>

  def tflatMap[R: Tuple](next: L => Directive[R]): Directive[R] = {
    Directive[R] { inner =>
      self.tapply(value => next(value).tapply(inner))
    }
  }

  def tmap[R: Tuple](f: L => R): Directive[R] =
    Directive[R] { inner =>
      self.tapply(value => inner(f(value)))
    }

  def &[R](magnet: ConjunctionMagnet[L]): magnet.Out = magnet(this)

  def |[U >: L](other: Directive[L]): Directive[L] = {
    Directive[L] { inner => (ctx, previous, state) =>
      self.tapply(value => inner(value))(ctx, previous, state).flatMap {
        case complete: RouteResult.Complete => EventStream.fromValue(complete, emitOnce = true)
        case RouteResult.Rejected           => other.tapply(value => inner(value))(ctx, previous, state)
      }
    }

  }

  def tcollect[R: Tuple](f: PartialFunction[L, R]): Directive[R] =
    Directive[R] { inner =>
      self.tapply(
        value =>
          if (f.isDefinedAt(value)) {
            inner(f(value))
          } else {
            directives.reject
          }
      )
    }

  def tfilter(predicate: L => Boolean): Directive[L] =
    Directive[L] { inner =>
      self.tapply(
        value =>
          if (predicate(value)) {
            inner(value)
          } else {
            directives.reject
          }
      )
    }

}

object Directive {

  def apply[L: Tuple](f: (L => Route) => Route): Directive[L] = {
    new Directive[L](
      inner =>
        (ctx, previous, state) =>
          f(
            value =>
              (ctx, previous, state) => {
                inner(value)(ctx, previous, state.setValue(value))
              }
          )(ctx, previous, state)
    )
  }

  implicit def toDirective[L: Tuple](route: Route): Directive[L] =
    Directive[L](_ => route)

  implicit def addDirectiveApply[L: Tuple](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In => Route =
    subRoute =>
      (ctx, previous, state) => {
        val result = directive.tapply(hac(subRoute))(ctx, previous, state)
        result
      }

  implicit def addNullaryDirectiveApply(directive: Directive0): Route => Route =
    subRoute =>
      (ctx, previous, state) => {
        val result = directive.tapply(_ => subRoute)(ctx, previous, state)
        result
      }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R](f: L => R): Directive1[R] =
      underlying.tmap { case Tuple1(value) => Tuple1(f(value)) }

    def flatMap[R](f: L => Directive1[R]): Directive1[R] =
      underlying.tflatMap { case Tuple1(value) => f(value) }

    def collect[R: Tuple](f: PartialFunction[L, R]): Directive[R] =
      underlying.tcollect {
        case Tuple1(value) => f(value)
      }

    def filter(description: String)(predicate: L => Boolean): Directive1[L] =
      underlying.tfilter { case Tuple1(value) => predicate(value) }

    def signal: Directive1[Signal[L]] =
      new Directive[Tuple1[Signal[L]]]({ inner => (ctx, previous, state) =>
        underlying.tapply {
          value => // TODO figure this out, when this is run, enter is not yet called
            (ctx, previous, state) =>
              previous.getValue[Var[L]](state.path) match {
                case None =>
                  val var$ = Var(value._1)
                  inner(Tuple1(var$.signal))(ctx, previous, state.setValue(var$))
                case Some(var$) =>
                  var$.writer.onNext(value._1)
                  inner(Tuple1(var$.signal))(ctx, previous, state.setValue(var$))
              }
        }(ctx, previous, state)
      })

  }

}
