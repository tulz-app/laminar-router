package app.tulz.routing
import app.tulz.routing.util.{ApplyConverter, Tuple}
import com.raquo.airstream.signal.{Signal, Var}

class Directive[L](
  val tapply: (L ⇒ Route) => Route
)(implicit val ev: Tuple[L]) {
  self =>

  def tflatMap[R: Tuple](next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R] { inner ⇒
      self.tapply(value ⇒ next(value).tapply(inner))
    }
  }

  def tmap[R: Tuple](f: L ⇒ R): Directive[R] =
    Directive[R] { inner ⇒
      self.tapply(value ⇒ inner(f(value)))
    }

  def &[R](magnet: ConjunctionMagnet[L]): magnet.Out = magnet(this)

  def tcollect[R: Tuple](f: PartialFunction[L, R]): Directive[R] =
    Directive[R] { inner ⇒
      self.tapply(
        value ⇒
          if (f.isDefinedAt(value)) {
            inner(f(value))
          } else {
            directives.reject
          }
      )
    }

  def tfilter(predicate: L => Boolean): Directive[L] =
    Directive[L] { inner ⇒
      self.tapply(
        value ⇒
          if (predicate(value)) {
            inner(value)
          } else {
            directives.reject
          }
      )
    }

}

object Directive {

  def apply[L: Tuple](f: (L ⇒ Route) ⇒ Route): Directive[L] = {
    new Directive[L](
      inner =>
        (ctx, rctx) => {
          val result = f(
            value =>
              (ctx, rctx) => {
                rctx.reportNewValue(value)
                inner(value)(ctx, rctx)
              }
          )(ctx, rctx)
          result
        }
    )
  }

  implicit def toDirective[L: Tuple](route: Route): Directive[L] =
    Directive[L](_ => route)

  implicit def addDirectiveApply[L: Tuple](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In ⇒ Route =
    f ⇒
      (ctx, rctx) => {
        rctx.enter('/')
        val result = directive.tapply(hac(f))(ctx, rctx)
        rctx.leave()
        result
      }

  implicit class NullaryDirectiveExt(val directive: Directive0) extends AnyRef {

    def apply(subRoute: Route): Route = { (ctx, rctx) =>
      rctx.enter('/')
      val result = directive.tapply(_ => subRoute)(ctx, rctx)
      rctx.leave()
      result
    }

  }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R](f: L ⇒ R): Directive1[R] =
      underlying.tmap { case Tuple1(value) ⇒ Tuple1(f(value)) }

    def flatMap[R](f: L ⇒ Directive1[R]): Directive1[R] =
      underlying.tflatMap { case Tuple1(value) ⇒ f(value) }

    def collect[R: Tuple](f: PartialFunction[L, R]): Directive[R] =
      underlying.tcollect {
        case Tuple1(value) ⇒ f(value)
      }

    def filter(description: String)(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter { case Tuple1(value) ⇒ predicate(value) }

    def signal: Directive1[Signal[L]] =
      new Directive[Tuple1[Signal[L]]]({ inner => (ctx, rctx) =>
        underlying.tapply { value ⇒ // TODO figure this out, when this is run, enter is not yet called
          rctx.previousValue[Var[L]] match {
            case None =>
              val var$ = Var(value._1)
              rctx.reportNewValue(var$)
              inner(Tuple1(var$.signal))
            case Some(var$) =>
              rctx.reportNewValue(var$)
              var$.writer.onNext(value._1)
              inner(Tuple1(var$.signal))
          }
        }(ctx, rctx)
      })

  }

}
