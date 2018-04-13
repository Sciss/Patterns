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

import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object Event {
  final val empty = Event(Map.empty)

  implicit object serializer extends ImmutableSerializer[Event] {

    def read(in: DataInput): Event = {
      val b = Map.newBuilder[String, Any]
      var sz = in.readInt()
      b.sizeHint(sz)
      val ref = if (sz <= 1) null else new PatElem.RefMapIn
      while (sz > 0) {
        val k = in.readUTF()
        val v = PatElem.read(in, ref)
        b += (k -> v)
        sz -= 1
      }
      val coll = b.result()
      Event(coll)
    }

    def write(e: Event, out: DataOutput): Unit = {
      val coll = e.map
      out.writeInt(coll.size)
      var ref = new PatElem.RefMapOut
      coll.foreach { case (k, v) =>
        out.writeUTF(k)
        ref = PatElem.write(v, out, ref)
      }
    }
  }

  private def getOrElseDouble(out: Event, key: String, default: => Double): Double = {
    val opt = out.map.get(key)
    opt match {
      case Some(d: Double)  => d
      case Some(i: Int)     => i.toDouble
      case _                => default
    }
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

  /** SoundProcesses specific */
  final val keyValue      = "value"

  def scale(out: Event): Scale =
    out.map.get(keyScale).fold(Scale.default) {
      case Some(s: Scale) => s
      case _              => Scale.default
    }

  import de.sciss.numbers.Implicits._

  def mTranspose  (out: Event): Double = getOrElseDouble(out, keyMTranspose , 0)
  def gTranspose  (out: Event): Double = getOrElseDouble(out, keyGTranspose , 0.0)
  def cTranspose  (out: Event): Double = getOrElseDouble(out, keyCTranspose , 0.0)

  def octave      (out: Event): Double = getOrElseDouble(out, keyOctave     , 5.0)
  def root        (out: Event): Double = getOrElseDouble(out, keyRoot       , 0.0)
  def degree      (out: Event): Double = getOrElseDouble(out, keyDegree     , 0)

  def detune      (out: Event): Double = getOrElseDouble(out, keyDetune     , 0.0)
  def harmonic    (out: Event): Double = getOrElseDouble(out, keyHarmonic   , 1.0)

  def note        (out: Event): Double = getOrElseDouble(out, keyNote       , {
    val _scale = scale(out)
    _scale.degreeToKey(degree(out) + mTranspose(out))
  })

  def midiNote    (out: Event): Double = getOrElseDouble(out, keyMidiNote   , {
    val _scale = scale(out)
    val temp = (note(out) + gTranspose(out) + root(out)) / _scale.stepsPerOctave + octave(out) - 5.0
    temp * (12.0 * _scale.octaveRatio.log2) + 60.0
  })

  def detunedFreq (out: Event): Double = getOrElseDouble(out, keyDetunedFreq, freq(out) + detune(out))

  def freq        (out: Event): Double = getOrElseDouble(out, keyFreq       , {
    (midiNote(out) + cTranspose(out)).midicps * harmonic(out)
  })

  def dur         (out: Event): Double = getOrElseDouble(out, keyDur        , 0.0)
  def stretch     (out: Event): Double = getOrElseDouble(out, keyStretch    , 1.0)
  def legato      (out: Event): Double = getOrElseDouble(out, keyLegato     , 0.8)
  def delta       (out: Event): Double = getOrElseDouble(out, keyDelta      , stretch(out) * dur(out))
  def sustain     (out: Event): Double = getOrElseDouble(out, keySustain    , delta(out) * legato(out))

  def db          (out: Event): Double = getOrElseDouble(out, keyDb         , -20.0)
  def amp         (out: Event): Double = getOrElseDouble(out, keyAmp        , db(out).dbamp)
  def pan         (out: Event): Double = getOrElseDouble(out, keyPan        , 0.0)
}
//trait Event extends CTop {
//  type COut = Event.Out // Map[String, _]
//}
final case class Event(map: Map[String, Any]) extends Iterable[(String, Any)] {
  def + (kv: (String, Any)): Event = copy(map = map + kv)

  def iterator: Iterator[(String, Any)] = map.iterator
}