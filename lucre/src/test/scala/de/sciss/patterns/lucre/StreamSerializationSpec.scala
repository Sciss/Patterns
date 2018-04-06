package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.patterns.Types.{Widen, Widen2}
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

  "Stream serialization" should "work" in { sys =>
    val g = Graph {
      import graph._
//      implicitly[Widen[Int, Double]]
      implicitly[Widen2[Int, Double, Double]]
      Brown(70, 120, 3).sqrt // midicps
    }

    val c = Context[S]
    c.expand(g)
  }
}
