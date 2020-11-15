package de.sciss.patterns

import de.sciss.lucre.{InMemory, IntObj, StringObj, Folder => LFolder}
import de.sciss.patterns.lucre.PatImport._
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.proc.Pattern
import org.scalatest.matchers.should.Matchers

class AttrSpec extends DurableSpec with Matchers {
  "Primitive attributes" should "work" in { implicit sys =>
    val res = sys.step { implicit tx =>
      val e = Graph {
        val i = "int".attr[Int]
        i
      }
      val p = Pattern.newConst[T](e)
      p.attr.put("int", IntObj.newConst(1234))

      implicit val ctx: LContext[T, InMemory.Txn] = LContext.dual(p)(tx, tx.inMemoryBridge)
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
      val p = Pattern.newConst[T](e)
      val f = LFolder[T]()
      f.addLast(StringObj .newConst("ignore"))
      f.addLast(IntObj    .newConst(1234))
      f.addLast(IntObj    .newConst(5678))
      f.addLast(StringObj .newConst("ignore"))
      p.attr.put("folder", f)

      implicit val ctx: LContext[T, InMemory.Txn] = LContext.dual(p)(tx, tx.inMemoryBridge)
      ctx.expandDual(e).toIterator(ctx, tx.inMemory).take(3).toList
    }
    assert(res === List(1234, 5678))
  }}
