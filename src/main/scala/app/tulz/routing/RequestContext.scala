package app.tulz.routing
import app.tulz.cookies.Cookies
import com.raquo.airstream.core.Observer
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.eventstream.EventStream
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.Signal
import org.scalajs.dom
import org.scalajs.dom.PopStateEvent
import org.scalajs.dom.raw.Location

import scala.scalajs.js

final case class RequestContext(
  unmatchedPath: List[String],
  params: Map[String, Seq[String]],
  cookies: Map[String, String],
) {

  def withUnmatchedPath(path: List[String]): RequestContext = this.copy(unmatchedPath = path)

}

class RequestContextSignal(
  $locations: Signal[List[String]],
  $params: Signal[Map[String, Seq[String]]],
  $cookies: Signal[Map[String, String]],
  $cookieRefresh: Observer[Unit]
) {

  val signal: Signal[RequestContext] =
    $locations.combineWith($params).combineWith($cookies).map {
      case ((path, params), cookies) => RequestContext(path, params, cookies)
    }

  def refreshCookies(): Unit = $cookieRefresh.onNext( () )

}

object RequestContext {

  def signal(includeCookies: Option[Set[String]])(implicit owner: Owner): RequestContextSignal = {
    val (cookiesSignal, cookieRefreshObserver) = cookies(includeCookies)
    new RequestContextSignal(
      locations,
      params,
      cookiesSignal,
      cookieRefreshObserver
    )
  }

  private def loc(location: Location): List[String] = {
    location.pathname.dropWhile(_ == '/').split('/').toList.dropWhile(_.isEmpty)
  }

  private def fromEvent[E](target: dom.EventTarget, event: String): EventStream[E] = {
    val bus = new EventBus[E]
    val eventHandler: js.Function1[E, Unit] = (e: E) => {
      bus.writer.onNext(e)
    }
    target.addEventListener(event, eventHandler)
    bus.events
  }

  private def extractParams(location: Location): Map[String, Seq[String]] = {
    val vars   = location.search.dropWhile(_ == '?').split('&')
    val result = scala.collection.mutable.Map[String, Seq[String]]()
    vars.foreach { entry =>
      entry.split('=') match {
        case Array(key, value) =>
          val decodedKey   = js.URIUtils.decodeURIComponent(key)
          val decodedValue = js.URIUtils.decodeURIComponent(value)
          result(decodedKey) = result.getOrElse(decodedKey, Seq.empty) :+ decodedValue
        case _ =>
      }
    }
    result.toMap
  }


  def locations(implicit owner: Owner): Signal[List[String]] =
    fromEvent[PopStateEvent](dom.window, "popstate")
      .map(_ => dom.window.location)
      .map(loc)
      .toSignal(loc(dom.window.location))

  def params(implicit owner: Owner): Signal[Map[String, Seq[String]]] =
    fromEvent[PopStateEvent](dom.window, "popstate")
      .map(_ => dom.window.location)
      .map(extractParams)
      .toSignal(extractParams(dom.window.location))

  def cookies(include: Option[Set[String]]): (Signal[Map[String, String]], Observer[Unit]) = {
    val bus = new EventBus[Unit]()
    val signal = bus.events.fold(Cookies.list(include))( (_, _) => Cookies.list(include))
    signal -> bus.writer
  }


}