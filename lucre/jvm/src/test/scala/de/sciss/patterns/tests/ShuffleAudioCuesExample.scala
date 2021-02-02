package de.sciss.patterns.tests

import de.sciss.audiofile.AudioFile
import de.sciss.file._
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{Cursor, StringObj, Folder => LFolder}
import de.sciss.patterns.lucre.PatImport._
import de.sciss.patterns.graph._
import de.sciss.patterns.{Event, Graph}
import de.sciss.synth.SynthGraph
import de.sciss.proc.{AuralContext, ObjKeys, Pattern, Transport, AudioCue => PAudioCue, Proc => PProc}

object ShuffleAudioCuesExample extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  protected def run[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]): Unit =
    new ShuffleAudioCuesExample[T]
}
class ShuffleAudioCuesExample[T <: Txn[T]](implicit cursor: Cursor[T])
  extends AuralTestLike[T] {

  protected def run()(implicit context: AuralContext[T]): Unit = {
    val pat   = Graph {
      val f     = "folder".attr[Folder]
      val cues  = f.collect[AudioCue]
      val rnd   = cues.shuffle
      val dur   = rnd.duration.<|(_.poll("dur"))

      Bind(
        Event.keyDelta    -> dur,
        Event.keyPlay     -> "proc",
        PProc.graphAudio  -> rnd
      )
    }

    val patH = cursor.step { implicit tx =>
//      implicit val system: S = tx.system
      val patObj: Pattern[T] = Pattern.newConst[T](pat)
      val procObj     = PProc[T]()
      procObj.graph() = SynthGraph {
        import de.sciss.synth._
        import de.sciss.synth.proc.graph
        import de.sciss.synth.ugen._
        val sig   = graph.VDiskIn  .ar(PProc.graphAudio)
        val amp   = graph.Attribute.kr(ObjKeys.attrGain, 1.0)
        val out   = sig * amp
        Out.ar(0, out)
      }
      patObj.attr.put("proc", procObj)
      val fObj      = LFolder[T]()
      fObj.addLast(StringObj.newConst("Ignore"))
      val dirIn     = file("/data/projects/Maeanderungen/audio_work/edited")
      dirIn.children(_.name.startsWith("MT-24_HH_No")).sorted(File.NameOrdering).foreach { fIn =>
        val spec    = AudioFile.readSpec(fIn)
        val cueVal  = PAudioCue(fIn.toURI, spec, offset = 0L, gain = 1.0)
        val cueObj  = PAudioCue.Obj.newConst[T](cueVal)
        fObj.addLast(cueObj)
      }
      patObj.attr.put("folder", fObj)

//      val ctx = Context.dual[S](patObj) // (patObj)
//      val stream = ctx.expandDual(pat)
//      while ({ val res = ctx.hasNext(stream); /* println(s"hasNext? $res"); */ res }) {
//        val evt = ctx.next(stream)
//        println(evt)
//      }
      tx.newHandle(patObj)
    }

    cursor.step { implicit tx =>
      val t = Transport[T](context.universe)
      val patObj = patH()
      t.addObject(patObj)
      t.play()
    }

    stopAndQuit(60.0)
  }
}
