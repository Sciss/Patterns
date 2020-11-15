/*
 *  AudioCueNumFramesImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.Exec
import de.sciss.serial.DataInput
import de.sciss.proc

object AudioCueNumFramesImpl extends StreamFactory {
  final val typeId = 0x41436E66 // "ACnf"

  def expand[T <: Exec[T], A](pat: graph.AudioCue.NumFrames)(implicit ctx: Context[T], tx: T): Stream[T, Long] = {
    import pat._
    val inStream = in.expand[T]
    new StreamImpl[T](inStream = inStream)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val inStream  = Stream.read[T, graph.AudioCue](in)
    new StreamImpl[T](inStream = inStream)
  }

  private final class StreamImpl[T <: Exec[T]](inStream: Stream[T, graph.AudioCue])

    extends AudioCueStreamImpl[T, Long](inStream) {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Long] = {
      val inStreamOut = c(inStream)
      new StreamImpl[Out](inStream = inStreamOut)
    }

    protected def typeId: Int = AudioCueNumFramesImpl.typeId

    protected def mapCue(cue: proc.AudioCue): Long = cue.numFrames
  }
}
