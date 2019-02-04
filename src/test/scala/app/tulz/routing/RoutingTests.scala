package app.tulz.routing

import com.raquo.airstream.core.Observer
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.{Signal, Var}
import utest._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.timers._
import directives._
import scala.collection.breakOut

object RoutingTests extends TestSuite {

  implicit val testOwner: Owner = new Owner {}

  case class Page(p: String)
  case class PageWithSignal($segment: Signal[String])

  abstract class WithRoute(route: (String => Unit) => Route) {
    val requestContext = new TestRequestContext()
    val probe          = new ListBuffer[String]()

    val sub = runRoute(
      route((s: String) => {
        println(s"probe: $s")
        probe.append(s)
      }),
      requestContext.signal
    )

  }

  val tests = Tests {

    "routing" - {

      "simple pathEnd" - new WithRoute(
        probe =>
          pathEnd {
            complete {
              probe("end")
            }
          }
      ) {
        def route =
          requestContext.path()
        probe.toList ==> List("end")
        sub.kill()
      }

      "alternate path" - new WithRoute(
        probe =>
          path("a") {
            complete {
              Future.successful(probe("a"))
            }
          } ~
            path("b") {
              complete {
                Future.successful(probe("b"))
              }
            }
      ) {
        requestContext.path("b")
        requestContext.path("a")
        probe.toList ==> List("b", "a")
        sub.kill()
      }

      "deep alternate path" - new WithRoute(
        probe =>
          pathPrefix("prefix1") {
            pathPrefix("prefix2") {
              pathEnd {
                complete {
                  probe("prefix1/prefix2")
                }
              } ~
                path("suffix1") {
                  complete {
                    probe("prefix1/prefix2/suffix1")
                  }
                }
            }
          } ~
            pathPrefix("prefix2") {
              pathPrefix("prefix3") {
                pathEnd {
                  complete {
                    probe("prefix2/prefix3")
                  }
                } ~
                  path("suffix2") {
                    complete {
                      probe("prefix2/prefix3/suffix2")
                    }
                  } ~
                  path("suffix3") {
                    param('param1) { paramValue =>
                      complete {
                        probe(s"prefix2/prefix3/suffix3?param1=$paramValue")
                      }
                    }
                  }
              }
            }
      ) {
        requestContext.path("prefix2", "prefix3", "suffix2")
        requestContext.path("prefix1", "prefix2")
        requestContext.path("prefix1", "prefix2", "suffix1")
        requestContext.path("prefix2", "prefix3")
        requestContext.path("prefix2", "prefix3", "suffix3")
        requestContext.params('param1 -> "param-value")

        probe.toList ==> List(
          "prefix2/prefix3/suffix2",
          "prefix1/prefix2",
          "prefix1/prefix2/suffix1",
          "prefix2/prefix3",
          "prefix2/prefix3/suffix3?param1=param-value",
        )
        sub.kill()
      }
    }

//    "pathEnd works" - {
//      val route = pathEnd.map(_ => "end").mapTo(Page("end"))
//      * - {
//        val $pages = runRoute(route, singleLoc(), $context)
//        nSignals(1, $pages).map { p =>
//          p ==> Some(Page("end")) :: Nil
//        }
//      }
//    }
//
//    "sub signal works" - {
//      val route = pathPrefix("prefix").sub { _ =>
//        path(segment).signal.map { $segment =>
//          PageWithSignal($segment)
//        }
//      }
//      * - {
//        val $locs = generateSignals(
//          List(
//            loc("prefix", "1"),
//            loc("prefix", "2"),
//            loc("prefix", "3"),
//            loc("prefix", "2"),
//            loc("prefix2", "2"),
//            loc("prefix", "5"),
//            loc("prefix", "6"),
//            loc("prefix", "7"),
//            loc("prefix", "6")
//          )
//        )
//        val $pages = runRoute(route, $locs, $context)
//        nthSignal(3, $pages).flatMap {
//          case Some(page) =>
//            nSignals(4, page.$segment).map { subLocs =>
//              subLocs ==> List(
//                "5",
//                "6",
//                "7",
//                "6"
//              )
//            }
//          case None => Future.failed(new RuntimeException("no prefix match"))
//        }
//      }
//    }

  }

  def nthSignal[T](n: Int, s: Signal[T], waitTime: Long = 1000): Future[T] = {
    val p     = Promise[T]()
    var count = n
    s.addObserver(Observer { t =>
      if (count >= 0) {
        count = count - 1
        if (count == 0) {
          p.success(t)
        }
      }
    })

    setTimeout(waitTime) {
      if (!p.isCompleted) {
        p.failure(new RuntimeException("nthSignal timeout"))
      }
    }
    p.future
  }

  def nSignals[T](n: Int, s: Signal[T], waitTime: Long = 1000): Future[List[T]] = {
    val p     = Promise[List[T]]()
    var count = n
    var list  = List.empty[T]
    s.addObserver(Observer { t =>
      if (count >= 0) {
        count = count - 1
        list = t :: list
        if (count == 0) {
          p.success(list.reverse)
        }
      }
    })

    setTimeout(waitTime) {
      if (!p.isCompleted) {
        p.failure(new RuntimeException("nSignals timeout"))
      }
    }
    p.future
  }

  def generateSignals[T](s: List[T], interval: Long = 10): Signal[T] = s match {
    case head :: rest =>
      val $var = Var(head)
      var ss   = rest
      def doNext(): Unit = ss match {
        case h :: tail =>
          ss = tail
          $var.writer.onNext(h)
          setTimeout(interval) {
            doNext()
          }
        case _ =>
      }
      setTimeout(interval) {
        doNext()
      }
      $var.signal
    case _ => ???
  }

}

class TestRequestContext {

  private val pathBus    = Var[List[String]](List.empty)
  private val paramsBus  = Var[Map[String, List[String]]](Map.empty)
  private val cookiesBus = Var[Map[String, String]](Map.empty)

  val signal: Signal[RequestContext] =
    pathBus.signal.combineWith(paramsBus.signal).combineWith(cookiesBus.signal).map {
      case ((path, params), cookies) => RequestContext(path, params, cookies)
    }

  def path(parts: String*): Unit = {
    println(s"path: ${parts.toList}")
    pathBus.writer.onNext(parts.toList)
  }

  def params(params: (Symbol, String)*): Unit = {
    paramsBus.writer.onNext(
      params
        .groupBy(_._1)
        .toList
        .map {
          case (name, values) =>
            name.name -> values.map(_._2).toList
        }(breakOut)
    )
  }

}
