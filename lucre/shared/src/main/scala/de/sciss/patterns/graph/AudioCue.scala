/*
 *  AudioCue.scala
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
package graph

import de.sciss.lucre.{Adjunct, Exec, Txn, Obj => LObj}
import de.sciss.patterns.stream.{AudioCueNumChannelsImpl, AudioCueNumFramesImpl, AudioCueSampleRateImpl}
import de.sciss.serial.DataInput
import de.sciss.proc

object AudioCue extends Obj.Adjunct[AudioCue] with Adjunct.Factory {
//  def typeId: Int = proc.AudioCue.typeId

//  type Repr[T <: Txn[T]] = proc.AudioCue.Obj[T]

  final val id: Int = 0x481

  implicit def tpe: Obj.Adjunct[AudioCue] = this

  override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

  def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[AudioCue] = obj match {
    case ao: proc.AudioCue.Obj[T] => Some(AudioCue(ao.value))
    case _                        => None
  }

//  def translate[T <: Txn[T]](obj: proc.AudioCue.Obj[T])(implicit tx: T): AudioCue = {
//    val peer = obj.value
//    AudioCue(peer)
//  }

  final case class NumFrames(in: Pat[AudioCue]) extends Pattern[Long] {
    def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Long] =
      AudioCueNumFramesImpl.expand(this)

    def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Long] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }

    override def productPrefix: String = s"AudioCue$$NumFrames"
  }

  final case class NumChannels(in: Pat[AudioCue]) extends Pattern[Int] {
    def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Int] =
      AudioCueNumChannelsImpl.expand(this)

    def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Int] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }

    override def productPrefix: String = s"AudioCue$$NumChannels"
  }

  final case class SampleRate(in: Pat[AudioCue]) extends Pattern[Double] {
    def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Double] =
      AudioCueSampleRateImpl.expand(this)

    def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Double] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }

    override def productPrefix: String = s"AudioCue$$SampleRate"
  }
}
@deprecated("Should not need a wrapper, complicates EventAsRunnerMap", since = "0.19.1")
final case class AudioCue(peer: proc.AudioCue) // extends Obj