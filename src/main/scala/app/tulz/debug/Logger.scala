package app.tulz.debug

trait Logger {

  def trace(message: String, extra: Any*): Unit
  def debug(message: String, extra: Any*): Unit
  def info(message: String, extra: Any*): Unit
  def error(message: String, extra: Any*): Unit

}
