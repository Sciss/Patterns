package de.sciss.patterns.lucre

import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralContext, AuralObj, AuralSystem, Confluent, Durable, Proc, SoundProcesses, SynthGraphObj, TimeRef, Timeline, WorkspaceHandle, showAuralLog, showTransportLog}

import scala.concurrent.stm.Txn

object AuralTestLike {
  trait Factory {
    val confluent = false /* true */  // currently test4 has a problem with event-variables in confluent

    protected def run[S <: Sys[S]](name: String)(implicit cursor: stm.Cursor[S]): Unit

    def init(args: Array[String]): Unit = {
      SoundProcesses.init()
      Pattern       .init()

      val name = args.headOption.getOrElse("?")

      if (confluent) {
        type S  = Confluent
        val sys = Confluent(BerkeleyDB.tmp())
        val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
        run[S](name)(cursor)

      } else {
        type S  = Durable
        val sys = Durable(BerkeleyDB.tmp())
        val cursor: stm.Cursor[S] = sys
        run[S](name)(cursor)
      }
    }
  }
}
abstract class AuralTestLike[S <: Sys[S]](implicit cursor: stm.Cursor[S]) {
  // ---- abstract ----

  protected def run()(implicit context: AuralContext[S]): Unit

  // ---- impl ----

  showAuralLog      = true
  showTransportLog  = true
  // de.sciss.lucre.synth.showLog = true

  final val as: AuralSystem = AuralSystem()
  cursor.step { implicit tx =>
    as.whenStarted(initView)
    as.start()
  }

  final def initView(s: Server): Unit = {
    if (Txn.findCurrent.isDefined) {
      Console.err.println("Damn! I could swear there is no transaction.")
      throw new IllegalStateException()
    }

    s.peer.dumpOSC()
    implicit val context: AuralContext[S] = cursor.step { implicit tx =>
      import WorkspaceHandle.Implicits._
      AuralContext[S](s)
    }

    run()
  }

  final def after(secs: Double, latency: Boolean = false)(code: S#Tx => Unit)
                 (implicit context: AuralContext[S]): Unit = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep((secs * 1000).toLong)
        if (latency) context.scheduler.stepTag { implicit tx =>
          code(tx)
        }
        else cursor.step { implicit tx =>
          code(tx)
        }
      }
    }
    Txn.findCurrent.fold(t.start()) { implicit tx =>
      Txn.afterCommit(_ => t.start())
    }
  }

  final def quit()(implicit tx: S#Tx): Unit =
    tx.afterCommit {
      Thread.sleep(1000)  // have to wait a bit for scsynth to quit
      scala.sys.exit()
    }

  final def procV(graph: => Unit)(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val pObj  = proc(graph)
    val _view = AuralObj.Proc(pObj)
    _view
  }

  final def proc(graph: => Unit)(implicit tx: S#Tx): Proc[S] = {
    val p = Proc[S]
    val g = SynthGraph {
      graph
    }
    p.graph() = SynthGraphObj.newConst[S](g)
    p // Obj(Proc.Elem(p))
  }

  final def timelineV()(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val tlObj = timeline()
    val _view = AuralObj.Timeline(tlObj)
    _view
  }

  final def timeline()(implicit tx: S#Tx): Timeline[S] = {
    val tl    = Timeline[S]
    tl // Obj(Timeline.Elem(tl))
  }

  final def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  final def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  final def putDouble(proc: Proc[S], key: String, value: Double)(implicit tx: S#Tx): Unit = {
    // val imp = ExprImplicits[S]
    // import imp._
    proc.attr.put(key, value: DoubleObj[S])
  }

  final def stopAndQuit(delay: Double = 4.0)(implicit context: AuralContext[S]): Unit =
    after(delay) { implicit tx =>
      as.stop()
      quit()
    }
}
