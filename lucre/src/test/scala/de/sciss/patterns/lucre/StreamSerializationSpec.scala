package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.serial.{DataInput, DataOutput}
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

    val values = sys.step { implicit tx =>
      implicit val c: Context[S] = Context[S]
      val stream0: Stream[S, Double] = c.expand(g)

      val out = DataOutput()
      stream0.write(out)
      val arr = out.toByteArray
      val in = DataInput(arr)
      val stream1 = Stream.read[S, Double](in, ())
      stream1.toIterator.take(10).toList
    }

    println(values)
  }
}
