package app.tulz.routing

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import app.tulz.routing.TupleComposition.Composition
import app.tulz.routing.util.Tuple

abstract class PathMatcher[T](implicit val tuple: Tuple[T]) {
  self =>

  def apply(path: List[String]): Either[(String, List[String]), (T, List[String])]

  def tmap[V: Tuple](f: T => V): PathMatcher[V] = new PathMatcher[V] {
    override def apply(in: List[String]): Either[(String, List[String]), (V, List[String])] =
      self(in).map {
        case (t, out) => f(t) -> out
      }
  }

  def tflatMap[V: Tuple](f: T => PathMatcher[V]): PathMatcher[V] = new PathMatcher[V] {
    override def apply(path: List[String]): Either[(String, List[String]), (V, List[String])] =
      self(path).flatMap {
        case (t, out) => f(t).apply(out)
      }
  }

  def tfilter(f: T => Boolean): PathMatcher[T] = this.tflatMap { t =>
    if (f(t)) {
      PathMatcher.tprovide(t)
    } else {
      PathMatcher.fail("filter failed")
    }
  }

  def tcollect[V: Tuple](f: PartialFunction[T, V]): PathMatcher[V] = this.tflatMap { t =>
    if (f.isDefinedAt(t)) {
      PathMatcher.tprovide(f(t))
    } else {
      PathMatcher.fail("collect failed")
    }
  }

  def withFilter(f: T => Boolean): PathMatcher[T] = this.tfilter(f)

  def /[V](other: PathMatcher[V])(implicit compose: Composition[T, V]): PathMatcher[compose.C] =
    self.tflatMap { t1 =>
      other.tmap { v =>
        compose.gc(t1, v)
      }(Tuple.yes)
    }(Tuple.yes)

  def as[O: Tuple](f: T => O): PathMatcher[O] = self.tmap(f)

  def void: PathMatcher[Unit] = this.tmap(_ => ())

  def unary_!(): PathMatcher[Unit] = new PathMatcher[Unit]() {
    override def apply(path: List[String]): Either[(String, List[String]), (Unit, List[String])] =
      self(path) match {
        case Right((_, rest)) => Left("not !matched" -> rest)
        case Left((_, rest))  => Right(()            -> rest)
      }
  }

}

object PathMatcher {

  val unit: PathMatcher[Unit] = new PathMatcher[Unit]() {
    override def apply(path: List[String]): Either[(String, List[String]), (Unit, List[String])] =
      Right(() -> path)
  }

  def tprovide[V: Tuple](v: V): PathMatcher[V] = unit.tmap(_ => v)

  def provide[V](v: V): PathMatcher[Tuple1[V]] = tprovide(Tuple1(v))

  def fail[T: Tuple](msg: String): PathMatcher[T] = new PathMatcher[T]() {
    override def apply(path: List[String]): Either[(String, List[String]), (T, List[String])] =
      Left(msg -> path)
  }

}

object PathMatchers extends PathMatchers

trait PathMatchers {

  def segment: PathMatcher1[String] = new PathMatcher1[String] {

    override def apply(path: List[String]): Either[(String, List[String]), (Tuple1[String], List[String])] =
      path match {
        case head :: tail => Right(Tuple1(head)             -> tail)
        case Nil          => Left(s"unexpected end of path" -> Nil)
      }

  }

  def segment(s: String): PathMatcher0 = segment.tfilter(t => t._1 == s).void

  def regex(r: Regex): PathMatcher1[Match] =
    segment
      .tmap(s => Tuple1(r.findFirstMatchIn(s._1)))
      .tcollect {
        case Tuple1(Some(m)) => Tuple1(m)
      }

  def fromTry[V](t: Try[V]): PathMatcher1[V] = new PathMatcher1[V] {
    override def apply(path: List[String]): Either[(String, List[String]), (Tuple1[V], List[String])] =
      t match {
        case Success(value) =>
          Right(Tuple1(value) -> path)
        case Failure(exception) =>
          Left(exception.getMessage -> path)
      }
  }

  def tryParse[V](t: => V): PathMatcher1[V] = fromTry(Try(t))

  def long: PathMatcher1[Long] = segment.tflatMap { matched =>
    tryParse(matched._1.toLong)
  }

  def double: PathMatcher1[Double] = segment.tflatMap { matched =>
    tryParse(matched._1.toDouble)
  }

  implicit def stringToSegment(s: String): PathMatcher[Unit] = segment(s)

}
