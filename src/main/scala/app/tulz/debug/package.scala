package app.tulz
import scala.scalajs.js

package object debug {

  object unsafe {
    var logger: js.UndefOr[(String, Seq[Any]) => Unit] = js.undefined
  }

}
