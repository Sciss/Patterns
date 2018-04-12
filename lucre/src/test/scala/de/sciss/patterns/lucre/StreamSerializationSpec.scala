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

  def verify[A](thunk: => Pat[A])(implicit sys: S): Unit = {
    val seed = 666L

    val p = Graph(thunk)

    val values = {
      implicit val c: patterns.Context[Plain] = patterns.Context()
      c.setRandomSeed(seed)
      c.expand(p).toIterator.take(10).toList
    }

    implicit val c: Context[S] = sys.step { implicit tx => Context[S] }

    val stH = sys.step { implicit tx =>
      c.setRandomSeed(seed)
      val st: Stream[S, A] = c.expand(p)
//      tx.newHandle(st)
      val id = tx.newId()
      tx.newVar(id, st)
    }

    val trans = List(3, 3, 3, 1).flatMap { n =>
      sys.step { implicit tx =>
        val st = stH()
        List.fill(n) {
//          println("NEXT")
          st.next()
        }
      }
    }

    assert(values === trans)
  }

  import graph._

  // XXX TODO
//  "Apply serialization" should "work" in { implicit sys =>
//    verify {
//      Pat.loopWithIndex(11)(i => Apply(Pat(Pat(3), Pat(4), Pat(5)), i % 3))
//    }
//  }

  // Ok
//  "ArithmSeq serialization" should "work" in { implicit sys =>
//    verify {
//      Pat.loop(2)(ArithmSeq(20, ArithmSeq(0, 1).take(6)))
//    }
//  }

  "LoopWithIndex serialization" should "work" in { implicit sys =>
    verify {
      Pat.loopWithIndex(12)(identity)
    }
  }

//  "BinaryOp serialization" should "work" in { implicit sys =>
//    verify {
//      Pat.loopWithIndex(12) { i =>
////        val j = i + i - i * i
//        val k = ((i.toDouble / 1.5) % 4.0) mod 3.0
//        val m = (k min i) max 0.56
//        // m sig_== 1.0
//        m
//      }
//    }
//  }

  "Brown and UnaryOp serialization" should "work" in { implicit sys =>
    verify {
      Brown(70, 120, 3).midicps
    }
  }
}
