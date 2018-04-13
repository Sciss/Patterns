package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.patterns.graph.Constant
import org.scalatest.{Matchers, Outcome, fixture}

class EventSerializationSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = S

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "Event" should "be serializable" in { implicit sys =>
    sys.step { implicit tx =>
      val e = Event(Map("dur" -> Pat(1.0, 2.0, 3.0), "blah" -> Constant(-1)))
      val id = tx.newId()
      val vr = tx.newVar(id, e)
      val eo = vr()
      assert(e === eo)
    }
  }
}
