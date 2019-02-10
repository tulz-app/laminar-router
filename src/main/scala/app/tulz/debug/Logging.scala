package app.tulz.debug

object Logging {

  object level {

    val trace = 1
    val debug = 2
    val info = 3
    val error = 4

  }

  def trace(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(level.trace, message, extra))
  }

  def debug(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(level.debug, message, extra))
  }

  def info(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(level.info, message, extra))
  }

  def error(message: String, extra: Any*): Unit = {
    app.tulz.debug.unsafe.logger.foreach(_(level.error, message, extra))
  }

}
