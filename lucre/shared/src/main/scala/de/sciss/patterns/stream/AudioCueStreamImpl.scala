/*
 *  AudioCueStreamImpl.scala
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

package de.sciss.patterns.stream

import de.sciss.lucre.Exec
import de.sciss.patterns.{Context, Stream, graph}
import de.sciss.serial.DataOutput
import de.sciss.proc

abstract class AudioCueStreamImpl[T <: Exec[T], A](inStream: Stream[T, graph.AudioCue])
  extends Stream[T, A] {

  protected def mapCue(cue: proc.AudioCue): A

  final protected def writeData(out: DataOutput): Unit =
    inStream.write(out)

  final def dispose()(implicit tx: T): Unit =
    inStream.dispose()

  final def reset()(implicit tx: T): Unit =
    inStream.reset()

  final def hasNext(implicit ctx: Context[T], tx: T): Boolean =
    inStream.hasNext

  final def next()(implicit ctx: Context[T], tx: T): A = {
    val cue = inStream.next()
    mapCue(cue.peer)
  }
}