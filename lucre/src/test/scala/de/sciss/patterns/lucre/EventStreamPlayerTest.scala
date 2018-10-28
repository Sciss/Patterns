package de.sciss.patterns
package lucre

import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys
import de.sciss.patterns.graph._
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralContext, Transport, Proc => PProc}

object EventStreamPlayerTest extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  protected def run[S <: Sys[S]](name: String)(implicit cursor: Cursor[S]): Unit =
    new EventStreamPlayerTest[S]
}
class EventStreamPlayerTest[S <: Sys[S]](implicit cursor: Cursor[S])
  extends AuralTestLike[S] {

  protected def run()(implicit context: AuralContext[S]): Unit = {
    val pat   = Graph {
      val dur = Brown(2.0, 6.0, 1.0).reciprocal
      Bind(
        Event.keyDelta  -> dur,
        Event.keyPlay   -> "proc",
      )
    }

    val patH = cursor.step { implicit tx =>
      implicit val system: S = tx.system
      val patObj: Pattern[S] = Pattern.newConst[S](pat)
      val procObj     = PProc[S]()
      procObj.graph() = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.ugen._
        import de.sciss.synth.proc.graph.{Time, Duration}
        val time  = Time()
        val dur   = Duration()
        val freq  = (time % 10.0).linExp(0, 10, 300, 1000)
        val sig   = SinOsc.ar(freq) * Line.ar(1.0, 0.0, dur = dur)
        Out.ar(0, Pan2.ar(sig) * AmpCompA.ir(freq))
      }
      patObj.attr.put("proc", procObj)
      tx.newHandle(patObj)
    }

    cursor.step { implicit tx =>
      val t = Transport[S](context)
      val patObj = patH()
      t.addObject(patObj)
      t.play()
    }

    stopAndQuit(20.0)
  }
}
