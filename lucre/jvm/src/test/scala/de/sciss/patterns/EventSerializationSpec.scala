package de.sciss.patterns

import de.sciss.patterns.graph.{Constant, Pat}
import org.scalatest.matchers.should.Matchers

class EventSerializationSpec extends DurableSpec with Matchers {
  "Event" should "be serializable" in { implicit sys =>
    sys.step { implicit tx =>
      val e = Event(Map("dur" -> Pat(1.0, 2.0, 3.0), "blah" -> Constant(-1)))
      val id = tx.newId()
      val vr = id.newVar(e)
      val eo = vr()
      assert(e === eo)
    }
  }
}
