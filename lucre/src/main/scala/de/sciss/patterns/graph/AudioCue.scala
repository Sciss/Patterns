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

import de.sciss.lucre.stm.Base
import de.sciss.synth.proc

object AudioCue extends Obj.Type[AudioCue] {
  implicit def tpe: Obj.Type[AudioCue] = this

  def typeId: Int = proc.AudioCue.typeId

  final case class NumFrames(in: Pat[AudioCue]) extends Pattern[Long] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Long] = ???

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Long] = ???
  }

  final case class NumChannels(in: Pat[AudioCue]) extends Pattern[Int] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] = ???

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = ???
  }

  final case class SampleRate(in: Pat[AudioCue]) extends Pattern[Double] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Double] = ???

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Double] = ???
  }
}
trait AudioCue extends Obj {
  def value: proc.AudioCue
}
