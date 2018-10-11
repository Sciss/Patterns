package de.sciss.patterns.lucre

import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Durable, InMemory}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.patterns.Graph
import de.sciss.patterns.graph
import de.sciss.synth.proc.SoundProcesses
import org.scalatest.{Matchers, Outcome, fixture}

class AttrSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = S

  SoundProcesses.init()
  Pattern       .init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "Primitive attributes" should "work" in { implicit sys =>
    val res = sys.step { implicit tx =>
      val e = Graph {
        val i = "int".attr[Int]
        i
      }
      val p = Pattern.newConst[S](e)
      p.attr.put("int", IntObj.newConst(1234))

      implicit val ctx: Context[S, InMemory] = Context.dual(p)
      ctx.expandDual(e).toIterator(ctx, tx.inMemory).take(3).toList
    }
    assert(res === List(1234))
  }

  "Folder attributes" should "work" in { implicit sys =>
    val res = sys.step { implicit tx =>
      val e = Graph {
        import graph._
        val f = "folder".attr[Folder]
        val i = f.collect[Int]
        i
      }
      val p = Pattern.newConst[S](e)
      val f = stm.Folder[S]
      f.addLast(StringObj .newConst("ignore"))
      f.addLast(IntObj    .newConst(1234))
      f.addLast(IntObj    .newConst(5678))
      f.addLast(StringObj .newConst("ignore"))
      p.attr.put("folder", f)

      implicit val ctx: Context[S, InMemory] = Context.dual(p)
      ctx.expandDual(e).toIterator(ctx, tx.inMemory).take(3).toList
    }
    assert(res === List(1234, 5678))
  }}
