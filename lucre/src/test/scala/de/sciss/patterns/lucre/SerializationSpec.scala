package de.sciss.patterns.lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.patterns.Types.IntTop
import de.sciss.patterns.{Graph, Pat, graph}
import de.sciss.serial.{DataInput, DataOutput}
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
    val g = Graph[Int] {
      import graph._
      val in = Pat(1 to 4: _*).combinations(3)
      val pat = in.map { in: Pat[Int] =>
        in.drop(1)
      }
      Flatten(pat)
      //      Constant[IntTop](1)
    }
    val fH = cursor.step { implicit tx =>
      val f = Pattern.newVar[S](g)
//      f.graph() = g
      tx.newHandle(f) // -> g.sources.size
    }

    cursor.step { implicit tx =>
      val f = fH()
      val g1 = f.value
      assert(g1 === g)
//      assert(g1.sources.size === numSources )
//      assert(g1.sources      === g.sources  )
//      assert(g1.out          ==  g.out      ) // bloody triple-equals macro breaks
    }
  }

  "Another Pattern object" should "be serializable" in { _ =>
    val g = Graph {
      import graph._
      val b = Brown(lo = 10, hi = 40, step = 4)
      val c = Pat.flatFill(4) {
        b.take(10).distinct.sorted * 0.5
      }
      val d = c.drop(4).stutter(2)
      c ++ d
    }
    val out = DataOutput()
    Pattern.valueSerializer.write(g, out)
    val in = DataInput(out.toByteArray)
    val g1 = Pattern.valueSerializer.read(in)
    assert(g1 === g)
//    assert(g1.sources === g.sources)
//    assert(g1.out     ==  g.out    ) // bloody triple-equals macro breaks
  }

  "An Event Pattern" should "be serializable" in { _ =>
    val g = Graph {
      import graph._
      val b = Brown(lo = 10, hi = 40, step = 4)
      val c = b.size
      Bind("foo" -> b, "bar" -> c)
    }
    val out = DataOutput()
    Pattern.valueSerializer.write(g, out)
    val in = DataInput(out.toByteArray)
    val g1 = Pattern.valueSerializer.read(in)
    assert(g1 === g)
//    assert(g1.sources === g.sources)
//    assert(g1.out     ==  g.out    ) // bloody triple-equals macro breaks
  }
}