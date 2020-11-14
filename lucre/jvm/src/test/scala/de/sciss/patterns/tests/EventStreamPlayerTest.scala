package de.sciss.patterns.tests

import de.sciss.lucre.Cursor
import de.sciss.lucre.synth.Txn
import de.sciss.patterns.graph._
import de.sciss.patterns.lucre.Pattern
import de.sciss.patterns.{Event, Graph}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralContext, Transport, Proc => PProc}

object EventStreamPlayerTest extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  protected def run[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]): Unit =
    new EventStreamPlayerTest[T]
}
class EventStreamPlayerTest[T <: Txn[T]](implicit cursor: Cursor[T])
  extends AuralTestLike[T] {

  protected def run()(implicit context: AuralContext[T]): Unit = {
    val pat     = Graph {
      val dur   = Brown(2.0, 6.0, 1.0).reciprocal
      val freq  = Brown(70, 120, 2).midiCps
      Bind(
        Event.keyDelta  -> dur,
        Event.keyLegato -> 0.8,
        Event.keyFreq   -> freq,
        Event.keyPlay   -> "proc"
      )
    }

    val patH = cursor.step { implicit tx =>
//      implicit val system: S = tx.system
      val patObj: Pattern[T] = Pattern.newConst[T](pat)
      val procObj     = PProc[T]()
      procObj.graph() = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.proc.graph.Duration
        import de.sciss.synth.proc.graph.Ops.stringToControl
        import de.sciss.synth.ugen._
        val dur   = Duration()
        val freq  = "freq".ar
        val sig   = SinOsc.ar(freq) * Line.ar(1.0, 0.0, dur = dur)
        Out.ar(0, Pan2.ar(sig) * AmpCompA.ir(freq))
      }
      patObj.attr.put("proc", procObj)
      tx.newHandle(patObj)
    }

    cursor.step { implicit tx =>
      val t = Transport[T](context)
      val patObj = patH()
      t.addObject(patObj)
      t.play()
    }

    stopAndQuit(20.0)
  }
}
