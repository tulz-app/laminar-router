package app.tulz.routing

import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.Tuple
import scala.language.implicitConversions

trait ConjunctionMagnet[L] {
  type Out
  def apply(underlying: Directive[L]): Out
}

object ConjunctionMagnet {

  implicit def fromDirective[L, R](other: Directive[R])(implicit composition: Composition[L, R]): ConjunctionMagnet[L] { type Out = Directive[composition.C] } =
    new ConjunctionMagnet[L] {
      type Out = Directive[composition.C]
      def apply(underlying: Directive[L]): Directive[composition.C] =
        Directive[composition.C] { inner ⇒
          underlying.tapply { prefix ⇒
            other.tapply { suffix ⇒
              inner(composition.gc(prefix, suffix))
            }
          }
        }(Tuple.yes)
    }

}
