/*
 *  Tuning.scala
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

import de.sciss.numbers
import de.sciss.numbers.IntFunctions

import scala.collection.immutable.{IndexedSeq => Vec}

object Tuning {
  import numbers.Implicits._

//  def apply(tuning: Vec[Double], octaveRatio: Double, name: String = "Unknown Tuning"): Tuning =
//    new Tuning(tuning, octaveRatio, name)
  
  def default(stepsPerOctave: Int): Tuning = et(stepsPerOctave)

  def et(stepsPerOctave: Int = 12): Tuning =
    apply(etSteps(stepsPerOctave), 2.0, etName(stepsPerOctave))

  private def etSteps(stepsPerOctave: Int): Vec[Double] =
    Vector.tabulate(stepsPerOctave)(i => i * 12.0 / stepsPerOctave)

  private def etName(stepsPerOctave: Int): String = s"ET$stepsPerOctave"

  ////////////////

  //TWELVE-TONE TUNINGS
  val et12 = Tuning((0 to 11).map(_.toDouble), 2, "ET12")

  val pythagorean = Tuning(
    Vec(1d, 256/243d, 9/8d, 32/27d, 81/64d, 4/3d, 729/512d, 3/2d, 128/81d, 27/16d, 16/9d, 243/128d)
        .map(_.ratioMidi), 2, "Pythagorean")
  
  val just = Tuning(
    Vec(1d, 16/15d, 9/8d, 6/5d, 5/4d, 4/3d, 45/32d, 3/2d, 8/5d, 5/3d,9/5d, 15/8d)
      map(_.ratioMidi), 2, "5-Limit Just Intonation")
  
  val sept1 = Tuning(
    Vec(1d, 16/15d, 9/8d, 6/5d, 5/4d, 4/3d, 7/5d, 3/2d, 8/5d, 5/3d, 9/5d, 15/8d)
      map(_.ratioMidi), 2, "Septimal Tritone Just Intonation")
  
  val sept2 = Tuning(
    Vec(1d, 16/15d, 9/8d, 6/5d, 5/4d, 4/3d, 7/5d, 3/2d, 8/5d, 5/3d, 7/4d, 15/8d)
      map(_.ratioMidi), 2, "7-Limit Just Intonation")
  
  val mean4 = Tuning(
    Vec(0.0, 0.755, 1.93, 3.105, 3.86, 5.035, 5.79, 6.965, 7.72, 8.895, 10.07, 10.82),
    2, "Meantone, 1/4 Syntonic Comma")

  val mean5 = Tuning(
    Vec(0.0, 0.804, 1.944, 3.084, 3.888, 5.028, 5.832, 6.972, 7.776, 8.916, 10.056, 10.86),
    2, "Meantone, 1/5 Pythagorean Comma")

  val mean6 = Tuning(
    Vec(0, 0.86, 1.96, 3.06, 3.92, 5.02, 5.88, 6.98, 7.84, 8.94, 10.04, 10.9),
    2, "Meantone, 1/6 Pythagorean Comma")

  val kirnberger = Tuning(
    Vec(1d, 256/243d, 5d.sqrt /2d, 32/27d, 5/4d, 4/3d, 45/32d, 5.pow(0.25), 128/81d,
      5.pow(0.75)/2d, 16/9d, 15/8d).map(_.ratioMidi),
    2, "Kirnberger III")

  val werckmeister = Tuning(
    Vec(0.0, 0.92, 1.93, 2.94, 3.915, 4.98, 5.9, 6.965, 7.93, 8.895, 9.96, 10.935),
    2, "Werckmeister III")

  val vallotti = Tuning(
    Vec(0.0, 0.94135, 1.9609, 2.98045, 3.92180, 5.01955, 5.9218, 6.98045, 7.9609, 8.94135, 10.0, 10.90225),
    2, "Vallotti")

  val young = Tuning(
    Vec(0.0, 0.9, 1.96, 2.94, 3.92, 4.98, 5.88, 6.98, 7.92, 8.94, 9.96, 10.9),
    2, "Young")

  val reinhard = Tuning(
    Vec(1d, 14/13d, 13/12d, 16/13d, 13/10d, 18/13d, 13/9d, 20/13d, 13/8d, 22/13d, 13/7d, 208/105d)
      .map(_.ratioMidi), 2, "Mayumi Reinhard")

  val wcHarm = Tuning(
    Vec(1d, 17/16d, 9/8d, 19/16d, 5/4d, 21/16d, 11/8d, 3/2d, 13/8d, 27/16d, 7/4d, 15/8d)
      .map(_.ratioMidi), 2, "Wendy Carlos Harmonic")

  val wcSJ = Tuning(
    Vec(1d, 17/16d, 9/8d, 6/5d, 5/4d, 4/3d, 11/8d, 3/2d, 13/8d, 5/3d, 7/4d, 15/8d)
      .map(_.ratioMidi), 2, "Wendy Carlos Super Just")

  val lu = Tuning(
    Vec(1d, 2187/2048d, 9/8d, 19683/16384d, 81/64d, 177147/131072d, 729/612d, 3/2d, 6561/4096d,
      27/16d, 59049/32768d, 243/128d).map(_.ratioMidi), 2, "Chinese Shi-er-lu scale")

  //MORE THAN TWELVE-TONE ET
  val et19 = Tuning((0 to 18).map(_ * 12/19d), 2, "ET19")
  val et22 = Tuning((0 to 21).map(_ *  6/11d), 2, "ET22")
  val et24 = Tuning((0 to 23).map(_ *  1/ 2d), 2, "ET24")
  val et31 = Tuning((0 to 30).map(_ * 12/31d), 2, "ET31")
  val et41 = Tuning((0 to 40).map(_ * 12/41d), 2, "ET41")
  val et53 = Tuning((0 to 52).map(_ * 12/53d), 2, "ET53")

  //NON-TWELVE-TONE JI
  val johnston = Tuning(
    Vec(1d, 25/24d, 135/128d, 16/15d, 10/9d, 9/8d, 75/64d, 6/5d,
      5/4d, 81/64d, 32/25d, 4/3d, 27/20d, 45/32d, 36/25d, 3/2d, 25/16d, 8/5d, 5/3d,
      27/16d, 225/128d, 16/9d, 9/5d, 15/8d, 48/25d).map(_.ratioMidi), 2, "Ben Johnston")

  val partch = Tuning(
    Vec(1d, 81/80d, 33/32d, 21/20d, 16/15d, 12/11d, 11/10d, 10/9d, 9/8d,
      8/7d, 7/6d, 32/27d, 6/5d, 11/9d, 5/4d, 14/11d, 9/7d, 21/16d, 4/3d, 27/20d, 11/8d,
      7/5d, 10/7d, 16/11d, 40/27d, 3/2d, 32/21d, 14/9d, 11/7d, 8/5d, 18/11d, 5/3d, 27/16d,
      12/7d, 7/4d, 16/9d, 9/5d, 20/11d, 11/6d, 15/8d, 40/21d, 64/33d, 160/81d)
      .map(_.ratioMidi), 2, "Harry Partch")

  val catler = Tuning(
    Vec(1d, 33/32d, 16/15d, 9/8d, 8/7d, 7/6d, 6/5d, 128/105d, 16/13d,
      5/4d, 21/16d, 4/3d, 11/8d, 45/32d, 16/11d, 3/2d, 8/5d, 13/8d, 5/3d, 27/16d, 7/4d,
      16/9d, 24/13d, 15/8d).map(_.ratioMidi), 2, "Jon Catler")

  val chalmers = Tuning(
    Vec(1d, 21/20d, 16/15d, 9/8d, 7/6d, 6/5d, 5/4d, 21/16d, 4/3d, 7/5d,
      35/24d, 3/2d, 63/40d, 8/5d, 5/3d, 7/4d, 9/5d, 28/15d, 63/32d)
      .map(_.ratioMidi), 2, "John Chalmers")

  val harrison = Tuning(
    Vec(1d, 16/15d, 10/9d, 8/7d, 7/6d, 6/5d, 5/4d, 4/3d, 17/12d, 3/2d,
      8/5d, 5/3d, 12/7d, 7/4d, 9/5d, 15/8d).map(_.ratioMidi), 2, "Lou Harrison")

  val sruti = Tuning(
    Vec(1d, 256/243d, 16/15d, 10/9d, 9/8d, 32/27d, 6/5d, 5/4d, 81/64d,
      4/3d, 27/20d, 45/32d, 729/512d, 3/2d, 128/81d, 8/5d, 5/3d, 27/16d, 16/9d, 9/5d,
      15/8d, 243/128d).map(_.ratioMidi), 2, "Sruti")

  val perret = Tuning(
    Vec(1d, 21/20d, 35/32d, 9/8d, 7/6d, 6/5d, 5/4d, 21/16d, 4/3d, 7/5d, 35/24d,
      3/2d, 63/40d, 8/5d, 5/3d, 7/4d, 9/5d, 15/8d, 63/32d)
      .map(_.ratioMidi), 2, "Wilfrid Perret")

  val michaelHarrison = Tuning(
    Vec(1d, 28/27d, 135/128d, 16/15d, 243/224d, 9/8d, 8/7d, 7/6d,
      32/27d, 6/5d, 135/112d, 5/4d, 81/64d, 9/7d, 21/16d, 4/3d, 112/81d, 45/32d, 64/45d, 81/56d,
      3/2d, 32/21d, 14/9d, 128/81d, 8/5d, 224/135d, 5/3d, 27/16d, 12/7d, 7/4d, 16/9d, 15/8d,
      243/128d, 27/14d).map(_.ratioMidi), 2, "Michael Harrison 24 tone 7-limit")


  //HARMONIC SERIES -- length arbitrary
  val harmonic = Tuning((1 to 24).map(_.ratioMidi.toDouble), 2, "Harmonic Series 24")

  //STRETCHED/SHRUNK OCTAVE
  //Bohlen-Pierce
  val bp = Tuning((0 to 12).map(_ * (3.0.ratioMidi/13d)), 3.0, "Bohlen-Pierce")

  val wcAlpha = Tuning((0 to 14).map(_ * 0.78 ), (15 * 0.78 ).midiRatio, "Wendy Carlos Alpha")
  val wcBeta  = Tuning((0 to 18).map(_ * 0.638), (19 * 0.638).midiRatio, "Wendy Carlos Beta" )
  val wcGamma = Tuning((0 to 33).map(_ * 0.351), (34 * 0.351).midiRatio, "Wendy Carlos Gamma")
}
final case class Tuning(tuning: Vec[Double], octaveRatio: Double, name: String = "Unknown Tuning") {
  def size: Int = tuning.size

  def wrapAt(index: Int): Double = {
    import IntFunctions.wrap
    val indexW = wrap(index, 0, size - 1)
    tuning(indexW)
  }
}
