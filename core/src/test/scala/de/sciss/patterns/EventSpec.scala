package de.sciss.patterns

import org.scalatest.{FlatSpec, Matchers}

class EventSpec extends FlatSpec with Matchers {
  "An event" should "yield correct default values" in {
    val evt     = Event.Out(Map("dur" -> 0.5))
    val stretch = Event.stretch(evt)
    assert(stretch === 1.0)
    val dur     = Event.dur(evt)
    assert(dur === 0.5)
    val delta = Event.delta(evt)
    assert(delta === 0.5)
  }
}
