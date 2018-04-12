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
    val p = Graph(thunk)
    verifyGraph(p)
  }

  def verifyLoop[A](fun: Pat[Int] => Pat[A])(implicit sys: S): Unit = {
    val p = Graph {
      Pat.loopWithIndex(12)(fun)
    }
    verifyGraph(p)
  }

  def verifyGraph[A](p: Pat[A])(implicit sys: S): Unit = {
    val seed = 666L

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

  // N.B. ignored tests are those for which serialization currently fails,
  // and thus these cases need fixing.

  ignore should "work for Apply" in { implicit sys =>
    verify {
      Pat.loopWithIndex(11)(i => Apply(Pat(Pat(3), Pat(4), Pat(5)), i % 3))
    }
  }

  "Serialization" should "work for ArithmSeq" in { implicit sys =>
    verify {
      Pat.loop(2)(ArithmSeq(20, ArithmSeq(0, 1).take(6)))
    }
  }

  it should "work for BinaryOp" in { implicit sys =>
    verifyLoop { i =>
      val k = ((i.toDouble / 1.5) % 4.0) mod 3.0
      val m = (k min i) max 0.56
      m
    }
  }

  it should "work for Bind" in { implicit sys =>
    verifyLoop { i =>
      Bind("foo" -> i)
    }
  }

  it should "work for Brown and UnaryOp" in { implicit sys =>
    verifyLoop { i =>
      Brown(70, 120, i.hold().take(2)).midicps
    }
  }

  it should "work for Bubble" in { implicit sys =>
    verifyLoop { i =>
      i.squared.bubble
    }
  }

  it should "work for Cat and Hold" in { implicit sys =>
    verifyLoop { i =>
      val a = i.hold().take(3)
      a ++ a
    }
  }

  ignore should "work for Choose" in { implicit sys =>
    verifyLoop { i =>
      Pat(i, Pat(5), Pat(6), Pat(7)).choose
    }
  }

  it should "work for Combinations" in { implicit sys =>
    verifyLoop { i =>
      Pat(4, 5, 6, 7).combinations(i)
    }
  }

  it should "work for Differentiate" in { implicit sys =>
    verifyLoop { i =>
      ArithmSeq(0, i.hold().take(2)).differentiate
    }
  }

  it should "work for Distinct and Flatten" in { implicit sys =>
    verifyLoop { i =>
      Pat(i, Pat(5), Pat(6), Pat(7)).flatten.distinct
    }
  }

  it should "work for LoopWithIndex and Constant" in { implicit sys =>
    verify {
      Pat.loopWithIndex(12)(identity)
    }
  }

}
