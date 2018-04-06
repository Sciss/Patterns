package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import org.scalatest.{Matchers, Outcome, fixture}

class StreamSerializationSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = Durable

  // SoundProcesses.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "Stream serialization" should "work" in { implicit sys =>
    val g = Graph {
      import graph._
      Brown(70, 120, 3).midicps
    }

    sys.step { implicit tx =>
      val c = Context[S]
      c.expand(g)
    }
  }
}
