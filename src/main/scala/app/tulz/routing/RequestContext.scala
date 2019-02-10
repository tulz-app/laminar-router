package app.tulz.routing

import app.tulz.cookies.Cookies
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.raw.Location

import scala.scalajs.js

final case class RequestContext(
  unmatchedPath: List[String],
  params: Map[String, Seq[String]],
  cookies: Map[String, String]
) {

  def withUnmatchedPath(path: List[String]): RequestContext = this.copy(unmatchedPath = path)

}

class BrowserRequestContext(
  $locations: Signal[List[String]],
  $params: Signal[Map[String, Seq[String]]],
  $cookies: Signal[Map[String, String]],
  $cookieRefresh: Observer[Unit]
) {

  val signal: Signal[RequestContext] =
    $locations.combineWith($params).combineWith($cookies).map {
      case ((path, params), cookies) =>
        RequestContext(path, params, cookies)
    }

  def refreshCookies(): Unit = $cookieRefresh.onNext(())

}

object BrowserRequestContext {

  def signal(includeCookies: Option[Set[String]]): BrowserRequestContext = {
    val (cookiesSignal, cookieRefreshObserver) = cookies(includeCookies)
    val $locations                             = windowEvents.onPopState.map(_ =>
      extractPath(dom.window.location) -> extractParams(dom.window.location)
    ).toSignal(
      extractPath(dom.window.location) -> extractParams(dom.window.location)
    )
    new BrowserRequestContext(
      $locations.map(_._1),
      $locations.map(_._2),
      cookiesSignal,
      cookieRefreshObserver
    )
  }

  def extractPath(location: Location): List[String] = {
    location.pathname.dropWhile(_ == '/').split('/').toList.dropWhile(_.isEmpty)
  }

  def extractParams(location: Location): Map[String, Seq[String]] = {
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

  private def cookies(include: Option[Set[String]]): (Signal[Map[String, String]], Observer[Unit]) = {
    val bus    = new EventBus[Unit]()
    val signal = bus.events.fold(Cookies.list(include))((_, _) => Cookies.list(include))
    signal -> bus.writer
  }

}
