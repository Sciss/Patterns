package de.sciss.patterns
package lucre

import de.sciss.file._
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.graph._
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AuralContext, SynthGraphObj, Transport, AudioCue => PAudioCue, Proc => PProc}

object ShuffleAudioCuesExample extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  protected def run[S <: Sys[S]](name: String)(implicit cursor: Cursor[S]): Unit =
    new ShuffleAudioCuesExample[S]
}
class ShuffleAudioCuesExample[S <: Sys[S]](implicit cursor: Cursor[S])
  extends AuralTestLike[S] {

  protected def run()(implicit context: AuralContext[S]): Unit = {
    val pat   = Graph {
      val f     = "folder".attr[Folder] //  Folder("folder")
      val cues  = f.collect[AudioCue]
      val rnd   = cues.shuffle
      val dur   = rnd.duration

      Bind(
        "delta" -> dur,
        "proc"  -> "proc",
        "sig"   -> cues,
      )
    }

    val patH = cursor.step { implicit tx =>
      implicit val system: S = tx.system
      val patObj: Pattern[S] = Pattern.newConst[S](pat)
      implicit val ctx: patterns.Context[S] = Context[S] // (patObj)
      val it = pat.expand[S].toIterator
      println(s"hasNext? ${it.hasNext}")
      it.foreach { evt =>
        println(evt)
      }
      tx.newHandle(patObj)
    }

    cursor.step { implicit tx =>
      val procObj     = PProc[S]()
      procObj.graph() = SynthGraphObj.tape
      val patObj      = patH()
      patObj.attr.put("proc", procObj)

      val fObj      = stm.Folder[S]
      fObj.addLast(StringObj.newConst("Ignore"))
      val dirIn     = file("/data/projects/Maeanderungen/audio_work/edited")
      dirIn.children(_.name.startsWith("MT-24_HH_No")).sorted(File.NameOrdering).foreach { fIn =>
        val spec    = AudioFile.readSpec(fIn)
        val cueVal  = PAudioCue(fIn, spec, offset = 0L, gain = 1.0)
        val cueObj  = PAudioCue.Obj.newConst[S](cueVal)
        fObj.addLast(cueObj)
      }

      val t = Transport[S](context)
      t.addObject(patObj)
      t.play()
    }

    stopAndQuit(30.0)
  }
}
