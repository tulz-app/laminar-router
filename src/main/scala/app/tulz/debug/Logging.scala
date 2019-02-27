package app.tulz.debug

object Logging {

  private var sinks: Map[String, Seq[LogSink]] = Map.empty

  def addSink(logStreamName: String, minLevel: Int)(f: (String, Seq[Any]) => Unit): Unit = {
    val sink = new LogSink {
      override def message(level: Int, m: String, args: Any*): Unit =
        if (level >= minLevel) {
          f(m, args)
        }
    }
    sinks = sinks.updated(logStreamName, sinks.getOrElse(logStreamName, Seq.empty) :+ sink)
  }

  def getLogger(logStreamName: String): Logger = new Logger {

    def trace(message: String, extra: Any*): Unit = {
      sinks.getOrElse(logStreamName, Seq.empty).foreach(_.message(LogLevel.trace, message, extra))
    }

    def debug(message: String, extra: Any*): Unit = {
      sinks.getOrElse(logStreamName, Seq.empty).foreach(_.message(LogLevel.debug, message, extra))
    }

    def info(message: String, extra: Any*): Unit = {
      sinks.getOrElse(logStreamName, Seq.empty).foreach(_.message(LogLevel.info, message, extra))
    }

    def warn(message: String, extra: Any*): Unit = {
      sinks.getOrElse(logStreamName, Seq.empty).foreach(_.message(LogLevel.warn, message, extra))
    }

    def error(message: String, extra: Any*): Unit = {
      sinks.getOrElse(logStreamName, Seq.empty).foreach(_.message(LogLevel.error, message, extra))
    }

  }

}
