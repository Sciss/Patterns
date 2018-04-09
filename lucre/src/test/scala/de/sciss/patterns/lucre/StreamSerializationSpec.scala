package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Durable, Plain}
import de.sciss.patterns
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

    val seed = 666L

    val values = {
      implicit val c: patterns.Context[Plain] = patterns.Context()
      c.setRandomSeed(seed)
      c.expand(g).toIterator.take(10).toList
    }

    implicit val c: Context[S] = sys.step { implicit tx => Context[S] }

    val stH = sys.step { implicit tx =>
      c.setRandomSeed(seed)
      val st: Stream[S, Double] = c.expand(g)
      tx.newHandle(st)
    }

    val trans = List(3, 3, 3, 1).flatMap { n =>
      sys.step { implicit tx =>
        val st = stH()
        List.fill(n)(st.next())
      }
    }

    assert(values === trans)
  }
}
