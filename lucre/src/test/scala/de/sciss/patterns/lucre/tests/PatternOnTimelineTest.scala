package de.sciss.patterns.lucre.tests

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys
import de.sciss.patterns.Graph
import de.sciss.patterns.lucre.Pattern
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.proc.AuralContext

object PatternOnTimelineTest extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  def run[S <: Sys[S]](name: String)(implicit cursor: Cursor[S]): Unit =
    new PatternOnTimelineTest[S](name)
}
class PatternOnTimelineTest[S <: Sys[S]](name: String)(implicit cursor: stm.Cursor[S]) extends AuralTestLike[S] {
  protected def run()(implicit context: AuralContext[S]): Unit = test1()

  def test1()(implicit context: AuralContext[S]): Unit = {
    println("----pat on tl----")
    println(
      """
        |Expected behaviour:
        |A single (monophonic) sine impulse is heard
        |
        |""".stripMargin)

    val patVal = Graph {
      import de.sciss.patterns._
      import graph._
      val pch = Brown(60, 110, 2)
      val f = pch.midiCps
      Bind(
        "delta" -> 2.0, // 10.0, // 0.3,
        "play" -> "play",
        "freq" -> f
      )
    }

    val view = cursor.step { implicit tx =>
      val p = proc {
        import de.sciss.synth.proc.graph.Ops._
        import synth._
        import ugen._
        val f = "freq".kr(440f)
        val ln = Line.ar(0.5, 0.0, 0.25) // , doneAction = freeSelf)
        val osc = SinOsc.ar(f) * ln
        Out.ar(0, osc)
      }

      val pat = Pattern.newConst[S](patVal)
      //      pat.graph() = patVal
      pat.attr.put("play", p)

      val tl = timelineV()
      tl.obj.modifiableOption.get.add(Span(frame(0.1), frame(5.0)), pat)
      tl
    }

    val sch = context.universe.scheduler
    sch.stepTag { implicit tx =>
      println("--issue play--")
      view.play()

      stopAndQuit(5000.0)
    }
  }
}