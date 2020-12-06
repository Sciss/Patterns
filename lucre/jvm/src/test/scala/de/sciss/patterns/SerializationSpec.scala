package de.sciss.patterns

import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.Pat
import de.sciss.patterns.tests.RonTuplePure
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.proc.Pattern
import org.scalatest.matchers.should.Matchers

class SerializationSpec extends DurableSpec with Matchers {
  "A Pattern object" should "be serializable" in { cursor =>
    val g = Graph[Int] {
      import graph._
      val in = Pat(1 to 4: _*).combinations(3)
      val pat = in.map { (in: Pat[Int]) =>
        in.drop(1)
      }
      Flatten(pat)
      //      Constant[IntTop](1)
    }
    val fH = cursor.step { implicit tx =>
      val f = Pattern.newVar[T](g)
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

  def roundTrip(g: Pat[_]): Pat[_] = {
    val out = DataOutput()
    Pattern.valueFormat.write(g, out)
    val in = DataInput(out.toByteArray)
    Pattern.valueFormat.read(in)
  }

  "Another Pattern object" should "be serializable" in { _ =>
    val g = Graph {
      import graph._
      val b = Brown(lo = 10, hi = 40, step = 4)
      val c = Pat.loop(4) {
        b.take(10).distinct.sorted * 0.5
      }
      val d = c.drop(4).stutter(2)
      c ++ d
    }
    val g1 = roundTrip(g)
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
    val g1 = roundTrip(g)
    assert(g1 === g)
//    assert(g1.sources === g.sources)
//    assert(g1.out     ==  g.out    ) // bloody triple-equals macro breaks
  }

  "The tuple example" should "be serializable" in { _ =>
    val g = Graph {
      RonTuplePure.mkGraph()
    }
    val g1 = roundTrip(g)
    assert(g1 === g)
  }
}