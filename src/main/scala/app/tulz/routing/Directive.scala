package app.tulz.routing
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.TupleOps.Join
import app.tulz.routing.util.{ApplyConverter, Tuple, TupleOps, Tupler}
import com.raquo.airstream.signal.{Signal, Var}

import scala.language.implicitConversions

object Directive {

  implicit def toDirective[L: Tuple](route: Route): Directive[L] =
    Directive[L](_ => _ => route)

  def apply[L: Tuple](tapply: DirectivePath => (L ⇒ Route) ⇒ Route): Directive[L] = {
    println(s"creating directive...")
    new Directive[L] {
      override def tapply(parentPath: DirectivePath, inner: L => Route): Route =
        tapply(s"$parentPath/sub", inner)
    }
  }

  implicit def addDirectiveApply[L](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In ⇒ Route =
    f ⇒ directive.tapply("sub", hac(f))

  /**
    * Adds `apply` to Directive0. Note: The `apply` parameter is call-by-name to ensure consistent execution behavior
    * with the directives producing extractions.
    */
  implicit class NullaryDirectiveApply(val directive: Directive[Unit]) extends AnyRef {

    def apply(r: => Route): Route = {
      println("addByNameNullaryApply")
      val d = directive.tapply("sub", _ ⇒ r)
      println("addByNameNullaryApply done!")
      d
    }

  }

  implicit class SingleValueModifiers[L](underlying: Directive1[L]) extends AnyRef {

    def map[R](f: L ⇒ R): Directive1[R] =
      underlying.tmap { case Tuple1(value) ⇒ f(value) }

    def collect[R](f: PartialFunction[L, R]): Directive1[R] =
      underlying.tcollect {
        case Tuple1(value) ⇒ f(value)
      }

    def flatMap[R: Tuple](f: L ⇒ Directive[R]): Directive[R] = {
      println("flatMap")
      underlying.tflatMap("flatMap") { case Tuple1(value) ⇒ f(value) }
    }

    def filter(predicate: L ⇒ Boolean): Directive1[L] =
      underlying.tfilter({ case Tuple1(value) ⇒ predicate(value) })
  }

}

abstract class Directive[L](implicit val ev: Tuple[L]) {
  self =>

  protected def subId(path: String, suffix: String): String = s"$path/$suffix"

  def tapply(parentPath: DirectivePath, inner: L ⇒ Route): Route

  def tflatMap[R: Tuple](suffix: String)(next: L ⇒ Directive[R]): Directive[R] = {
    Directive[R] { path => inner ⇒
      self.tapply(path, value ⇒ next(value).tapply(subId(path, suffix), inner))
    }
  }

  def tmap[R](f: L ⇒ R): Directive1[R] =
    Directive[Tuple1[R]] { path => inner ⇒
      tapply(path, values ⇒ inner(Tuple1(f(values))))
    }

  def &(magnet: ConjunctionMagnet[L]): magnet.Out = magnet(this)

  def tcollect[R](f: PartialFunction[L, R]): Directive1[R] =
    self.tflatMap("collect") { t =>
      if (f.isDefinedAt(t)) {
        directives.tprovide(Tuple1(f(t)))
      } else {
        directives.reject
      }
    }

  def tfilter(predicate: L => Boolean): Directive[L] =
    self.tflatMap("filter") { t =>
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
    self.tflatMap("signal") { t =>
      Directive[Tuple1[Signal[L]]](
        path =>
          inner =>
            ctx =>
              rctx ⇒ {
                rctx.get[Var[L]](path) match {
                  case None =>
                    val var$ = Var(t)
                    rctx.reportNewValue(path, var$)
                    inner(Tuple1(var$.signal))(ctx)(rctx)
                  case Some(var$) =>
                    var$.writer.onNext(t)
                    inner(Tuple1(var$.signal))(ctx)(rctx)
                }
              }
      )
    }

}

trait ConjunctionMagnet[L] {
  type Out
  def apply(underlying: Directive[L]): Out
}

object ConjunctionMagnet {
  implicit def fromDirective[L, R](other: Directive[R])(implicit join: TupleOps.Join[L, R]): ConjunctionMagnet[L] { type Out = Directive[join.Out] } =
    new ConjunctionMagnet[L] {
      type Out = Directive[join.Out]
      def apply(underlying: Directive[L]) =
        Directive[join.Out] { path => inner ⇒
          underlying.tapply(path, prefix ⇒ other.tapply(s"$path&", suffix ⇒ inner(join(prefix, suffix))))
        }(Tuple.yes) // we know that join will only ever produce tuples
    }

}
