package de.sciss.patterns

import de.sciss.lucre.Adjunct.WidenToDouble
import de.sciss.lucre.Plain
import de.sciss.patterns.PatImport._
import de.sciss.patterns.graph.Pat
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.patterns.{Context => PContext, Stream => PStream}
import org.scalatest.matchers.should.Matchers

class StreamSerializationSpec extends DurableSpec with Matchers {
  def any2stringadd: Any = ()

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
      implicit val c: PContext[Plain] = PContext()
      c.setRandomSeed(seed)
      c.expand(p).toIterator.take(10).toList
    }

    implicit val c: PContext[T] = sys.step { implicit tx =>
      LContext[T]()
    }

    val stH = sys.step { implicit tx =>
      c.setRandomSeed(seed)
      val st: PStream[T, A] = c.expand(p)
      val id = tx.newId()
      id.newVar(st)
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

  // N.B. ignored tests are those for which serialization currently fails,
  // and thus these cases need fixing.

  it should "work for Apply" in { implicit sys =>
    verifyLoop { i =>
      Apply(Pat(3, 4, 5).bubble, i % 3)
    }
  }

  "Serialization" should "work for ArithmSeq" in { implicit sys =>
    verify {
      Pat.loop(2)(ArithmSeq(20, ArithmSeq(0, 1).take(6)))
    }
  }

  it should "work for BinaryOp" in { implicit sys =>
    verifyLoop { a =>
      val b = ((a + 1) - 2) * a
      val c = ((b / 3.0) % 4.0) mod 2.5
      val bool1 = c sig_== 0.0
      val bool2 = c sig_!= 0.0
      val bool3 = c > 0.5
      val bool4 = c < 0.5
      val bool5 = c >= 0.5
      val bool6 = c <= 0.5
      val bool7 = bool1.toInt | bool2.toInt & bool3.toInt | bool4.toInt ^ bool5.toInt | bool6.toInt
      val d     = bool7.toInt
      val e     = (c max d) min 5.0
      val f     = e.roundTo(0.1).roundUpTo(0.2).trunc(0.3)
      val g     = (f atan2 a).hypot(1.0).hypotApx(1.1).pow(3.0)
      val h     = g.difSqr(3.0).sumSqr(3.1).sqrSum(3.2).sqrDif(3.3).absDif(3.4)
      val i     = h.wrap2(1.0e7).fold2(1.0e3).excess(1.0e2).clip2(500)
      val j     = i.toInt << 1 >> 1 >>> 1
      val k     = (j gcd 24) lcm 5
      k
    }
  }

  it should "work for Bind" in { implicit sys =>
    verifyLoop { i =>
      Bind("foo" -> i)
    }
  }

  it should "work for Brown and UnaryOp" in { implicit sys =>
    verifyLoop { i =>
      Brown(70, 120, i.hold().take(2)).midiCps
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

  it should "work for Choose" in { implicit sys =>
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

  it should "work for Drop" in { implicit sys =>
    verifyLoop { i =>
      Pat(4, 5, 6, 7, 8, 9).drop(i)
    }
  }

  it should "work for LinLin, LinExp, ExpLin, ExpExp" in { implicit sys =>
    verifyLoop { i =>
      val j = (i + 1).hold()
      val p = Pat(4, 5, 6, 7, 8, 9)
      val q = p.toDouble
      val a = p.linLin(0, j, 0.0, 10.0)
      val b = p.linExp(0, j, 1.0, 10.0)
      val c = q.expLin(4.0, 9.0, 0, j)
      val d = q.expExp(4.0, 9.0, 1.0, 10.0)
      a + b + c + d
    }
  }

  it should "work for FlatMap" in { implicit sys =>
    verifyLoop { i =>
      i.bubble.flatMap { j =>
        j.hold().take(3).bubble.flatMap(identity)
      }
    }
  }

  it should "work for FoldLeft and Grouped" in { implicit sys =>
    verifyLoop { i =>
      Pat(4, 5, 6, 7, 8).grouped(2).foldLeft(i)(_ + _)
    }
  }

  it should "work for Format" in { implicit sys =>
    verifyLoop { i =>
      Format(Pat("Hallo %d", "Gallo %d"), i.hold())
    }
  }

  it should "work for Gate" in { implicit sys =>
    verifyLoop { i =>
      val j = i.hold()
      val p = Pat(4 to 12: _*)
      Gate(p, (p mod j) sig_== 0)
    }
  }

  it should "work for GeomSeq" in { implicit sys =>
    verify {
      Pat.loop(2)(GeomSeq(20, ArithmSeq(1, 1).take(6)))
    }
  }

  it should "work for IndexOfSlice and White" in { implicit sys =>
    verifyLoop { i =>
      White(-20, i.hold().cubed).indexOfSlice(i)
    }
  }

  it should "work for Indices" in { implicit sys =>
    verifyLoop { i =>
      White(1, 10).take(i).indices
    }
  }

  it should "work for Length" in { implicit sys =>
    verifyLoop { i =>
      White(1, 10).take(i).length
    }
  }

  it should "work for LoopWithIndex and Constant" in { implicit sys =>
    verifyLoop { _ =>
      Pat.loopWithIndex(3)(identity)
    }
  }

  it should "work for MapWithIndex" in { implicit sys =>
    verifyLoop { i =>
      Pat(3, 4, 5).bubble.mapWithIndex((x, xi) => (x max xi) << i)
    }
  }

  it should "work for Par" in { implicit sys =>
    verifyLoop { i =>
      val w = White(1.0, 4.0).take(i + 1)
      val p = w.grouped(10).map { dur =>
        Bind(Event.keyDur -> dur.roundTo(0.1))
      }
      Par(p)
    }
  }

  it should "work for PatMap" in { implicit sys =>
    verifyLoop { i =>
      Pat(3, 4, 5).bubble.map(x => x << i)
    }
  }

  it should "work for Poll" in { implicit sys =>
    verify {
      Pat.loopWithIndex(3) { i =>
        i.poll("i")
      } ++ Pat(-1).hold()
    }
  }

  it should "work for Shuffle" in { implicit sys =>
    verifyLoop { i =>
      Pat(4 to 40: _*).take(i).shuffle
    }
  }

  it should "work for Sliding" in { implicit sys =>
    verifyLoop { i =>
      Pat(4 to 12: _*).sliding((i + 1).hold(), 1)
    }
  }

  it should "work for Sorted" in { implicit sys =>
    verifyLoop { i =>
      White(1, 100).take(i absDif 7).sorted
    }
  }

  it should "work for SortWith" in { implicit sys =>
    verifyLoop { i =>
      White(1, 100).take(40).grouped(i).sortWith(_ > _)
    }
  }

  it should "work for Stutter" in { implicit sys =>
    verifyLoop { i =>
      Brown(1.0, 10.0, 1.0).stutter(i)
    }
  }

  it should "work for Sum" in { implicit sys =>
    verifyLoop { i =>
      ArithmSeq(0, 1).take(i).sum
    }
  }

  it should "work for Take" in { implicit sys =>
    verifyLoop { i =>
      Pat(4, 5, 6, 7, 8, 9).take(i)
    }
  }

  it should "work for Tap" in { implicit sys =>
    verify {
      Pat.loop(2) {
        Pat(4, 5) <| (_.poll("tap"))
      } ++ -1
    }
  }

  it should "work for Tuple2 and Zip2" in { implicit sys =>
    verifyLoop { i =>
      val tup = i zip i.sqrt
      val (_1, _2) = tup.unzip
      _1 * _2
    }
  }

  it should "work for UnaryOp" in { implicit sys =>
    verifyLoop { a =>
      val b = ~(-a).abs
      val c = b.toDouble
      val d: Pat[Double] = c.ceil.floor.squared.cubed
      implicitly[WidenToDouble[Int, Double]]
      val e = d.sqrt
      val f = e.log.exp
      val g = f.midiCps.cpsMidi.midiRatio.ratioMidi.dbAmp.ampDb.octCps.cpsOct.log2
      val h = g.log10.sin.cos.tan.sinh.cosh.tanh
      val i = h.atan.acos.asin
      val j = i.frac.reciprocal
      val k = j.rand2.signum.toInt
      val m = k.toDouble.rand.coin
      !m
    }
  }

  it should "work for Updated" in { implicit sys =>
    verifyLoop { i =>
      Pat(4 to 14: _*).updated(i, -1)
    }
  }

  it should "work for UpdatedAll" in { implicit sys =>
    verifyLoop { i =>
      Pat(4 to 14: _*).updatedAll(i ++ (i + 1), -1)
    }
  }

//  it should "work for Attribute" in { implicit sys =>
//    verifyLoop { i =>
//      val j = "foo".attr[Int](666)
//      j + i
//    }
//  }
}
