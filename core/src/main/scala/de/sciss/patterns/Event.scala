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

object Event {
//  type Out = Event#COut

//  object Out {
//    def newBuilder: mutable.Builder[(String, Any), Out] = ???
//  }
//  trait Out extends Map[String, Any] {
//    type K = String
//    type V = Any // Stream[Tx, Any]
//
//    override def + [V1 >: V](kv: (K, V1)): Out
//  }

  final case class Out(map: Map[String, Any]) {
    def + (kv: (String, Any)): Out = copy(map = map + kv)
  }

  private def getOrElseDouble(out: Out, key: String, default: => Double): Double =
    out.map.get(key).fold(default) {
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

  def scale(out: Out): Scale =
    out.map.get(keyScale).fold(Scale.default) {
      case Some(s: Scale) => s
      case _              => Scale.default
    }

  import de.sciss.numbers.Implicits._

  def mTranspose  (out: Out): Double = getOrElseDouble(out, keyMTranspose , 0)
  def gTranspose  (out: Out): Double = getOrElseDouble(out, keyGTranspose , 0.0)
  def cTranspose  (out: Out): Double = getOrElseDouble(out, keyCTranspose , 0.0)

  def octave      (out: Out): Double = getOrElseDouble(out, keyOctave     , 5.0)
  def root        (out: Out): Double = getOrElseDouble(out, keyRoot       , 0.0)
  def degree      (out: Out): Double = getOrElseDouble(out, keyDegree     , 0)

  def detune      (out: Out): Double = getOrElseDouble(out, keyDetune     , 0.0)
  def harmonic    (out: Out): Double = getOrElseDouble(out, keyHarmonic   , 1.0)

  def note        (out: Out): Double = getOrElseDouble(out, keyNote       , {
    val _scale = scale(out)
    _scale.degreeToKey(degree(out) + mTranspose(out))
  })

  def midiNote    (out: Out): Double = getOrElseDouble(out, keyMidiNote   , {
    val _scale = scale(out)
    val temp = (note(out) + gTranspose(out) + root(out)) / _scale.stepsPerOctave + octave(out) - 5.0
    temp * (12.0 * _scale.octaveRatio.log2) + 60.0
  })

  def detunedFreq (out: Out): Double = getOrElseDouble(out, keyDetunedFreq, freq(out) + detune(out))

  def freq        (out: Out): Double = getOrElseDouble(out, keyFreq       , {
    (midiNote(out) + cTranspose(out)).midicps * harmonic(out)
  })

  def dur         (out: Out): Double = getOrElseDouble(out, keyDur        , 0.0)
  def stretch     (out: Out): Double = getOrElseDouble(out, keyStretch    , 1.0)
  def legato      (out: Out): Double = getOrElseDouble(out, keyLegato     , 0.8)
  def sustain     (out: Out): Double = getOrElseDouble(out, keySustain    , dur(out) * legato(out) * stretch(out))

  def delta       (out: Out): Double = getOrElseDouble(out, keyDelta      , stretch(out) * dur(out))

  def db          (out: Out): Double = getOrElseDouble(out, keyDb         , -20.0)
  def amp         (out: Out): Double = getOrElseDouble(out, keyAmp        , db(out).dbamp)
  def pan         (out: Out): Double = getOrElseDouble(out, keyPan        , 0.0)
}
trait Event extends CTop {
  type COut = Event.Out // Map[String, _]
}
