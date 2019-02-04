package app.tulz.routing
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.{ApplyConverter, Tuple}
import com.raquo.airstream.signal.{Signal, Var}

import scala.language.implicitConversions
import scala.scalajs.js

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
    f ⇒ directive.tapply("a", hac(f))

  implicit class NullaryDirectiveExt(val directive: Directive0) extends AnyRef {

    def apply(subRoute: Route): Route = {
      directive.tapply("a", _ => subRoute)
    }

  }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R: Tuple](f: L ⇒ R): Directive[R] =
      underlying.tmap { case Tuple1(value) ⇒ f(value) }

    def collect[R](f: PartialFunction[L, R]): Directive1[R] =
      underlying.tcollect {
        case Tuple1(value) ⇒ f(value)
      }

    def flatMap[R: Tuple](suff: Option[String])(f: L ⇒ Directive[R]): Directive[R] = {
      underlying.tflatMap(suff) { case Tuple1(value) ⇒ f(value) }
    }

    def filter(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter({ case Tuple1(value) ⇒ predicate(value) })

    def signal: Directive1[Signal[L]] =
      underlying.tflatMap(null) { t =>
        Directive[Tuple1[Signal[L]]](
          path =>
            inner =>
              (ctx, rctx) ⇒ {
                val signalPath = underlying.subId(path, "signal")
                println(s"path: $path")
                println(s"signal path: $signalPath")
                rctx.get[Var[L]](signalPath) match {
                  case None =>
                    val var$ = Var(t._1)
                    println(s"signal initial value: ${t._1}")
                    rctx.reportNewValue(signalPath, var$)
                    inner(Tuple1(var$.signal))(ctx, rctx)
                  case Some(var$) =>
                    var$.writer.onNext(t._1)
                    rctx.undoChanged(path)
                    println(s"signal new value: ${t._1}")
                    inner(Tuple1(var$.signal))(ctx, rctx)
                }
              }
        )
      }

  }

}

abstract class Directive[L](implicit val ev: Tuple[L]) {
  self =>

  protected def subId(path: String, suffix: String): DirectivePath = s"$path/$suffix"

  def tapply(path: DirectivePath, inner: L ⇒ Route): Route = {
    _tapply(path, l => inner(l))
  }

  def _tapply(pathPrefix: DirectivePath, inner: L ⇒ Route): Route

  def tflatMap[R: Tuple](suff: Option[String])(next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R](
      path =>
        inner ⇒
          self.tapply(
            path,
            value ⇒
              next(value).tapply(
                suff.map(subId(path, _)).getOrElse(path),
                inner
              )
          )
    )
  }

  def tmap[R: Tuple](f: L ⇒ R): Directive[R] =
    Directive[R](
      path => inner ⇒ self.tapply(path, value ⇒ inner(f(value)))
    )

  def &[R](next: Directive[R])(implicit composition: Composition[L, R]): Directive[composition.C] =
    self.tflatMap(Some("&")) { l =>
      next.tmap { r =>
        composition.gc(l, r)
      }(Tuple.yes)
    }(Tuple.yes)

  def tcollect[R](f: PartialFunction[L, R]): Directive1[R] =
    self.tflatMap(Some("col")) { t =>
      if (f.isDefinedAt(t)) {
        directives.tprovide(Tuple1(f(t)))
      } else {
        directives.reject
      }
    }

  def tfilter(predicate: L => Boolean): Directive[L] =
    self.tflatMap(Some("flt")) { t =>
      if (predicate(t)) {
        directives.tprovide(t)
      } else {
        directives.reject
      }
    }

  def reportValue: Directive[L] =
    self.tflatMap(None) { value =>
      Directive[L](
        path =>
          inner =>
            (ctx, rctx) => {
              rctx.reportNewValue(path, value)
              inner(value)(ctx, rctx)
            }
      )
    }

//  def maybeOverrideC[U >: L](overrideWith: PartialFunction[RequestContext, U]): Directive[U] = new MaybeOverrideCDirective[L, U](self, overrideWith)
//
//  def maybeOverride[U >: L](overrideWith: RequestContext => Option[U]): Directive[U] = new MaybeOverrideDirective[L, U](self, overrideWith)

//  def mapTo[R](newValue: => R): Directive[R] =
//    self.tmap(_ => newValue)

}
