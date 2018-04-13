package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Durable, InMemory, Plain}
import de.sciss.patterns
import org.scalatest.{Matchers, Outcome, fixture}

class DebugPar extends fixture.FlatSpec with Matchers {
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
      val st = c.expand(p)
      val res = st.toIterator.take(10).toList
      res
    }

    implicit val c: Context[S] = sys.step { implicit tx => Context[S] }

    val stH = sys.step { implicit tx =>
      c.setRandomSeed(seed)
      val st: Stream[S, A] = c.expand(p)
      val id = tx.newId()
      tx.newVar(id, st)
    }

    val trans = List(3, 3, 3, 1).flatMap { n =>
      sys.step { implicit tx =>
        val st = stH()
        List.fill(n) {
          st.next()
        }
      }
    }

    assert(values === trans)
  }

  import graph._

  "Par" should "work as expected" in { implicit sys =>
    verifyLoop { i =>
      val w = White(1.0, 4.0).take(i + 1)
      val p = w.grouped(10).map { dur =>
        Bind(Event.keyDur -> dur.roundTo(0.1))
      }
      Par(p)
    }
  }
}
