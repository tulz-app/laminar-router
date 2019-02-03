package app.tulz.routing

import com.raquo.airstream.core.Observer
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.eventstream.EventStream
import com.raquo.airstream.ownership.Owner
import com.raquo.airstream.signal.{Signal, Val, Var}
import utest._
import directives._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.timers._

object RoutingTests extends TestSuite {

  implicit val testOwner: Owner = new Owner {}

  case class Context()
  case class Page(p: String)
  case class PageWithSignal($segment: Signal[String])

  val tests = Tests {

    "routing works" - {
      val probe = new ListBuffer[String]()
      val route = pathEnd {
        completeNow {
          probe.append("end")
        }
      }
      println(s"route created!")
      * - {
        val testContexts = new TestRequestContext()
        val sub = runRoute(route, testContexts.signal)
        testContexts.path()
        probe.toList ==> List("end")
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

  private val pathBus = Var[List[String]](List.empty)
  private val paramsBus = Var[Map[String, List[String]]](Map.empty)
  private val cookiesBus = Var[Map[String, String]](Map.empty)

  val signal: Signal[RequestContext] =
    pathBus.signal.combineWith(paramsBus.signal).combineWith(cookiesBus.signal).map {
      case ((path, params), cookies) => RequestContext(path, params, cookies)
    }

  def path(parts: String*): Unit = {
    pathBus.writer.onNext(parts.toList)
  }

}