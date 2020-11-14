package de.sciss.patterns.tests

import de.sciss.lucre.Cursor
import de.sciss.lucre.synth.Txn
import de.sciss.patterns.Graph
import de.sciss.patterns.lucre.Pattern
import de.sciss.synth
import de.sciss.synth.proc.AuralContext

object AuralPatternAttributeTest extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  def run[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]): Unit =
    new AuralPatternAttributeTest[T](name)
}
class AuralPatternAttributeTest[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]) extends AuralTestLike[T] {
  protected def run()(implicit context: AuralContext[T]): Unit = test1()

  def test1()(implicit context: AuralContext[T]): Unit = {
    println("----test1----")
    println(
      """
        |Expected behaviour:
        |A randomly walking pitch sequence is heard
        |
        |""".stripMargin)

    val patVal = Graph {
      import de.sciss.patterns._
      import graph._
      val b = Brown(lo = 60, hi = 100, step = 5)
//      Bind("value" -> b, Event.keyDur -> 0.5)
      Output(0.5, b)
    }

    val view = cursor.step { implicit tx =>
      val _view = procV {
        import de.sciss.synth.proc.graph.Ops._
        import synth._
        import ugen._
        val pitch   = "pitch".kr
        val osc     = SinOsc.ar(pitch.midiCps) * 0.25
        Out.ar(0, Pan2.ar(osc))
      }

      val pat = Pattern.newConst[T](patVal)
//      pat.graph() = patVal
      _view.obj.attr.put("pitch", pat)

      _view
    }

    val sch = context.universe.scheduler
    sch.stepTag { implicit tx =>
      println("--issue play--")
      view.play()

      after(8.0) { implicit tx =>
        println("--issue stop--")
        view.stop()
        after(1.0, latency = true) { implicit tx =>
          println("--issue play--")
          view.play()
          stopAndQuit(8)
        }
      }
    }
  }
}