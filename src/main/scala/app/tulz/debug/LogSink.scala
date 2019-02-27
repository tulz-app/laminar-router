package app.tulz.debug

trait LogSink {

  def message(level: Int, m: String, args: Any*): Unit

}
