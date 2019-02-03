package app.tulz.routing.util
import app.tulz.routing.Route

abstract class ApplyConverter[L] {
  type In
  def apply(f: In): L ⇒ Route
}

object ApplyConverter extends ApplyConverterInstances
