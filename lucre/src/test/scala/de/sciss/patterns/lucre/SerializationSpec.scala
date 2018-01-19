package de.sciss.patterns.lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.patterns.Types.IntTop
import de.sciss.patterns.{Graph, Pat, graph}
import de.sciss.synth.proc.SoundProcesses
import org.scalatest.{Matchers, Outcome, fixture}

class SerializationSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = S

  SoundProcesses.init()
  Pattern       .init()

  protected def withFixture(test: OneArgTest): Outcome = {
    val store  = BerkeleyDB.tmp()
    val system = Durable(store)
    try {
      test(system)
    } finally {
      system.close()
    }
  }

  "A Pattern object" should "be serializable" in { cursor =>
    val g = Graph[IntTop] {
      import graph._
      val in = Pseq(1 to 4).combinations(3)
      val pat = in.map { in: Pat.Int =>
        in.drop(1)
      }
      Flatten(pat)
//      Constant[IntTop](1)
    }
    val (fH, numSources) = cursor.step { implicit tx =>
      val f = Pattern[S]
      f.graph() = g
      tx.newHandle(f) -> g.sources.size
    }

    cursor.step { implicit tx =>
      val f = fH()
      val g1 = f.graph.value
      assert(g1.sources.size === numSources )
      assert(g1.sources      === g.sources  )
      assert(g1.out          ==  g.out      ) // bloody triple-equals macro breaks
    }
  }
}