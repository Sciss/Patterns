package de.sciss.patterns.tests

import de.sciss.log.Level
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Txn}
import de.sciss.lucre.{Cursor, DoubleObj}
import de.sciss.patterns.lucre.Pattern
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralContext, AuralObj, Confluent, Durable, Proc, SoundProcesses, TimeRef, Timeline, Universe}
import de.sciss.synth.proc.SoundProcesses.{logAural, logTransport}

import scala.concurrent.stm.Txn

object AuralTestLike {
  trait Factory {
    val confluent = false /* true */  // currently test4 has a problem with event-variables in confluent

    protected def run[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]): Unit

    def init(args: Array[String]): Unit = {
      SoundProcesses.init()
      Pattern       .init()

      val name = args.headOption.getOrElse("?")

      if (confluent) {
        type T  = Confluent.Txn
        val sys = Confluent(BerkeleyDB.tmp())
        val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
        run[T](name)(cursor)

      } else {
        type T  = Durable.Txn
        val sys = Durable(BerkeleyDB.tmp())
        val cursor: Cursor[T] = sys
        run[T](name)(cursor)
      }
    }
  }
}
abstract class AuralTestLike[T <: Txn[T]](implicit cursor: Cursor[T]) {
  // ---- abstract ----

  protected def run()(implicit context: AuralContext[T]): Unit

  // ---- impl ----

  logAural    .level = Level.Debug
  logTransport.level = Level.Debug
  // de.sciss.lucre.synth.showLog = true

  implicit val universe: Universe[T] = cursor.step { implicit tx => Universe.dummy }

  cursor.step { implicit tx =>
    val as = universe.auralSystem
    as.whenStarted(initView)
    as.start()
  }

  final def initView(s: Server): Unit = {
    if (Txn.findCurrent.isDefined) {
      Console.err.println("Damn! I could swear there is no transaction.")
      throw new IllegalStateException()
    }

    s.peer.dumpOSC()
    implicit val context: AuralContext[T] = cursor.step { implicit tx =>
      AuralContext[T](s)
    }

    run()
  }

  final def after(secs: Double, latency: Boolean = false)(code: T => Unit)
                 (implicit context: AuralContext[T]): Unit = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep((secs * 1000).toLong)
        if (latency) context.universe.scheduler.stepTag { implicit tx =>
          code(tx)
        }
        else cursor.step { implicit tx =>
          code(tx)
        }
      }
    }
    Txn.findCurrent.fold  (t.start()) { implicit tx =>
      Txn.afterCommit(_ => t.start())
    }
  }

  final def quit()(implicit tx: T): Unit =
    tx.afterCommit {
      Thread.sleep(1000)  // have to wait a bit for scsynth to quit
      scala.sys.exit()
    }

  final def procV(graph: => Unit)(implicit tx: T, context: AuralContext[T]): AuralObj.Proc[T] = {
    val pObj  = proc(graph)
    val _view = AuralObj.Proc(pObj)
    _view
  }

  final def proc(graph: => Unit)(implicit tx: T): Proc[T] = {
    val p = Proc[T]()
    val g = SynthGraph {
      graph
    }
    p.graph() = Proc.GraphObj.newConst[T](g)
    p // Obj(Proc.Elem(p))
  }

  final def timelineV()(implicit tx: T, context: AuralContext[T]): AuralObj.Timeline[T] = {
    val tlObj = timeline()
    val _view = AuralObj.Timeline(tlObj)
    _view
  }

  final def timeline()(implicit tx: T): Timeline.Modifiable[T] = {
    val tl = Timeline[T]()
    tl // Obj(Timeline.Elem(tl))
  }

  final def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  final def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  final def putDouble(proc: Proc[T], key: String, value: Double)(implicit tx: T): Unit = {
    // val imp = ExprImplicits[S]
    // import imp._
    proc.attr.put(key, value: DoubleObj[T])
  }

  final def stopAndQuit(delay: Double = 4.0)(implicit context: AuralContext[T]): Unit =
    after(delay) { implicit tx =>
      universe.auralSystem.stop()
      quit()
    }
}
