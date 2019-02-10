package app.tulz.debug

object Logging {

  def trace(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(message, extra))
  }

  def debug(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(message, extra))
  }

  def info(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(message, extra))
  }

  def error(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(message, extra))
  }

}
