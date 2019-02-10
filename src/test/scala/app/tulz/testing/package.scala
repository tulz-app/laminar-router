package app.tulz
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

package object testing {

  def delayedFuture[V](millis: Int)(value: => V): Future[V] =
    Future(value).delayed(millis)

  implicit class FutureExt[T](f: => Future[T]) {

    def delayed(millis: Int): Future[T] = {
      val promise = Promise[T]()
      f.onComplete { t =>
        js.timers.setTimeout(millis) {
          promise.complete(t)
        }
      }
      promise.future
    }
  }

}
