/*
 *  AudioCue.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.stm.{Base, Obj, Sys}
import de.sciss.patterns.stream.{AudioCueNumFramesImpl, AudioCueSampleRateImpl}
import de.sciss.synth.proc

object AudioCue extends Obj.Extractor[AudioCue] {
  def typeId: Int = proc.AudioCue.typeId

//  type Repr[S <: Sys[S]] = proc.AudioCue.Obj[S]

  implicit def tpe: Obj.Extractor[AudioCue] = this

  def extract[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Option[AudioCue] = obj match {
    case ao: proc.AudioCue.Obj[S] => Some(AudioCue(ao.value))
    case _                        => None
  }

//  def translate[S <: Sys[S]](obj: proc.AudioCue.Obj[S])(implicit tx: S#Tx): AudioCue = {
//    val peer = obj.value
//    AudioCue(peer)
//  }

  final case class NumFrames(in: Pat[AudioCue]) extends Pattern[Long] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Long] =
      AudioCueNumFramesImpl.expand(this)

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Long] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }
  }

  final case class NumChannels(in: Pat[AudioCue]) extends Pattern[Int] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = ???

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }
  }

  final case class SampleRate(in: Pat[AudioCue]) extends Pattern[Double] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Double] =
      AudioCueSampleRateImpl.expand(this)

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Double] = {
      val inT = t(in)
      if (inT.eq(in)) this else copy(in = inT)
    }
  }
}
final case class AudioCue(peer: proc.AudioCue) // extends Obj