package de.sciss.patterns.stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns.{Context, Stream, graph}
import de.sciss.serial.DataOutput
import de.sciss.synth.proc

abstract class AudioCueStreamImpl[S <: Base[S], A](inStream: Stream[S, graph.AudioCue])
  extends Stream[S, A] {

  protected def mapCue(cue: proc.AudioCue): A

  final protected def writeData(out: DataOutput): Unit =
    inStream.write(out)

  final def dispose()(implicit tx: S#Tx): Unit =
    inStream.dispose()

  final def reset()(implicit tx: S#Tx): Unit =
    inStream.reset()

  final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
    inStream.hasNext

  final def next()(implicit ctx: Context[S], tx: S#Tx): A = {
    val cue = inStream.next()
    mapCue(cue.peer)
  }
}