/*
 *  Event.scala
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

import de.sciss.patterns.Types.CTop
import de.sciss.patterns.graph.Constant

import scala.collection.mutable

object Event {
//  type Out = Event#COut

  object Out {
    def newBuilder: mutable.Builder[(String, Any), Out] = ???
  }
  trait Out extends Map[String, Any] {
    type K = String
    type V = Any // Stream[Tx, Any]

    override def + [V1 >: V](kv: (K, V1)): Out
  }

  private def getOrElseDouble(m: Out, key: String, default: => Double): Double =
    m.get(key).fold(default) {
      case Some(Constant(d: Double))  => d
      case Some(Constant(i: Int))     => i.toDouble
      case _                          => default
    }

  final val keyDelta      = "delta"
  final val keyStretch    = "stretch"
  final val keyDur        = "dur"

  final val keyMTranspose = "mtranspose"
  final val keyGTranspose = "gtranspose"
  final val keyCTranspose = "ctranspose"

  final val keyOctave     = "octave"
  final val keyRoot       = "root"
  final val keyDegree     = "degree"
  final val keyScale      = "scale"

  final val keyDetune     = "detune"
  final val keyHarmonic   = "harmonic"

  final val keyNote       = "note"
  final val keyMidiNote   = "midinote"
  final val keyFreq       = "freq"
  final val keyDetunedFreq= "detunedFreq"

  final val keySustain    = "sustain"
  final val keyLegato     = "legato"

  final val keyDb         = "db"
  final val keyAmp        = "amp"
  final val keyPan        = "pan"

  def scale(m: Out): Scale =
    m.get(keyScale).fold(Scale.default) {
      case Some(s: Scale) => s
      case _              => Scale.default
    }

  import de.sciss.numbers.Implicits._

  def mTranspose  (m: Out): Double = getOrElseDouble(m, keyMTranspose , 0)
  def gTranspose  (m: Out): Double = getOrElseDouble(m, keyGTranspose , 0.0)
  def cTranspose  (m: Out): Double = getOrElseDouble(m, keyCTranspose , 0.0)

  def octave      (m: Out): Double = getOrElseDouble(m, keyOctave     , 5.0)
  def root        (m: Out): Double = getOrElseDouble(m, keyRoot       , 0.0)
  def degree      (m: Out): Double = getOrElseDouble(m, keyDegree     , 0)

  def detune      (m: Out): Double = getOrElseDouble(m, keyDetune     , 0.0)
  def harmonic    (m: Out): Double = getOrElseDouble(m, keyHarmonic   , 1.0)

  def note        (m: Out): Double = getOrElseDouble(m, keyNote       , {
    val _scale = scale(m)
    _scale.degreeToKey(degree(m) + mTranspose(m))
  })

  def midiNote    (m: Out): Double = getOrElseDouble(m, keyMidiNote   , {
    val _scale = scale(m)
    val temp = (note(m) + gTranspose(m) + root(m)) / _scale.stepsPerOctave + octave(m) - 5.0
    temp * (12.0 * _scale.octaveRatio.log2) + 60.0
  })

  def detunedFreq (m: Out): Double = getOrElseDouble(m, keyDetunedFreq, freq(m) + detune(m))

  def freq        (m: Out): Double = getOrElseDouble(m, keyFreq       , {
    (midiNote(m) + cTranspose(m)).midicps * harmonic(m)
  })

  def dur         (m: Out): Double = getOrElseDouble(m, keyDur        , 0.0)
  def stretch     (m: Out): Double = getOrElseDouble(m, keyStretch    , 1.0)
  def legato      (m: Out): Double = getOrElseDouble(m, keyLegato     , 0.8)
  def sustain     (m: Out): Double = getOrElseDouble(m, keySustain    , dur(m) * legato(m) * stretch(m))

  def delta       (m: Out): Double = getOrElseDouble(m, keyDelta      , stretch(m) * dur(m))

  def db          (m: Out): Double = getOrElseDouble(m, keyDb         , -20.0)
  def amp         (m: Out): Double = getOrElseDouble(m, keyAmp        , db(m).dbamp)
  def pan         (m: Out): Double = getOrElseDouble(m, keyPan        , 0.0)
}
trait Event extends CTop {
  type COut = Event.Out // Map[String, _]
}
