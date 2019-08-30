/*
 *  AudioCueNumChannelsImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.stm.Base
import de.sciss.serial.DataInput
import de.sciss.synth.proc

object AudioCueNumChannelsImpl extends StreamFactory {
  final val typeId = 0x41436E63 // "ACnc"

  def expand[S <: Base[S], A](pat: graph.AudioCue.NumChannels)(implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = {
    import pat._
    val inStream = in.expand[S]
    new StreamImpl[S](inStream = inStream)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val inStream  = Stream.read[S, graph.AudioCue](in, access)
    new StreamImpl[S](inStream = inStream)
  }

  private final class StreamImpl[S <: Base[S]](inStream: Stream[S, graph.AudioCue])
    extends AudioCueStreamImpl[S, Int](inStream) {

    protected def typeId: Int = AudioCueNumChannelsImpl.typeId

    protected def mapCue(cue: proc.AudioCue): Int = cue.numChannels
  }
}