/*
 *  Scale.scala
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

import de.sciss.numbers.IntFunctions
import de.sciss.optional.Optional

import scala.collection.immutable.{IndexedSeq => Vec}

object Scale {
  val all: Map[String, Scale] = Map ()

  def apply(steps: Vec[Int], stepsPerOctave: Int, tuning: Optional[Tuning] = None, name: String): Scale =
    new Scale(steps, stepsPerOctave = stepsPerOctave,
      tuning = tuning.getOrElse(Tuning.default(stepsPerOctave)), name = name)

  // TWELVE TONES PER OCTAVE
  // 5 note scales
  val minorPentatonic = Scale(Vec(0,3,5,7,10), 12, name = "Minor Pentatonic")
  val majorPentatonic = Scale(Vec(0,2,4,7, 9), 12, name = "Major Pentatonic")
  // another mode of major pentatonic
  val ritusen         = Scale(Vec(0,2,5,7, 9), 12, name = "Ritusen")
    // another mode of major pentatonic
  val egyptian        = Scale(Vec(0,2,5,7,10), 12, name = "Egyptian")

  val kumoi           = Scale(Vec(0,2,3,7, 9), 12, name = "Kumai"     )
  val hirajoshi       = Scale(Vec(0,2,3,7, 8), 12, name = "Hirajoshi" )
  val iwato           = Scale(Vec(0,1,5,6,10), 12, name = "Iwato"     ) // mode of hirajoshi
  val chinese         = Scale(Vec(0,4,6,7,11), 12, name = "Chinese"   ) // mode of hirajoshi
  val indian          = Scale(Vec(0,4,5,7,10), 12, name = "Indian"    )
  val pelog           = Scale(Vec(0,1,3,7, 8), 12, name = "Pelog"     )

  val prometheus      = Scale(Vec(0,2,4,6,11), 12, name = "Prometheus")
  val scriabin        = Scale(Vec(0,1,4,7, 9), 12, name = "Scriabin"  )

  // han chinese pentatonic scales
  val gong            = Scale(Vec(0,2,4,7, 9), 12, name = "Gong"      )
  val shang           = Scale(Vec(0,2,5,7,10), 12, name = "Shang"     )
  val jiao            = Scale(Vec(0,3,5,8,10), 12, name = "Jiao"      )
  val zhi             = Scale(Vec(0,2,5,7, 9), 12, name = "Zhi"       )
  val yu              = Scale(Vec(0,3,5,7,10), 12, name = "Yu"        )

  // 6 note scales
  val whole           = Scale(0 to 10 by 2     , 12, name = "Whole Tone"  )
  val augmented       = Scale(Vec(0,3,4,7,8,11), 12, name = "Augmented"   )
  val augmented2      = Scale(Vec(0,1,4,5,8, 9), 12, name = "Augmented 2" )

  // Partch's Otonalities and Utonalities
  val partch_o1       = Scale(Vec(0, 8,14,20,25,34), 43, Tuning.partch, "Partch Otonality 1")
  val partch_o2       = Scale(Vec(0, 7,13,18,27,35), 43, Tuning.partch, "Partch Otonality 2")
  val partch_o3       = Scale(Vec(0, 6,12,21,29,36), 43, Tuning.partch, "Partch Otonality 3")
  val partch_o4       = Scale(Vec(0, 5,15,23,30,37), 43, Tuning.partch, "Partch Otonality 4")
  val partch_o5       = Scale(Vec(0,10,18,25,31,38), 43, Tuning.partch, "Partch Otonality 5")
  val partch_o6       = Scale(Vec(0, 9,16,22,28,33), 43, Tuning.partch, "Partch Otonality 6")
  val partch_u1       = Scale(Vec(0, 9,18,23,29,35), 43, Tuning.partch, "Partch Utonality 1")
  val partch_u2       = Scale(Vec(0, 8,16,25,30,36), 43, Tuning.partch, "Partch Utonality 2")
  val partch_u3       = Scale(Vec(0, 7,14,22,31,37), 43, Tuning.partch, "Partch Utonality 3")
  val partch_u4       = Scale(Vec(0, 6,13,20,28,38), 43, Tuning.partch, "Partch Utonality 4")
  val partch_u5       = Scale(Vec(0, 5,12,18,25,33), 43, Tuning.partch, "Partch Utonality 5")
  val partch_u6       = Scale(Vec(0,10,15,21,27,34), 43, Tuning.partch, "Partch Utonality 6")

  // hexatonic modes with no tritone
  val hexMajor7       = Scale(Vec(0,2,4,7,9,11), 12, name = "Hex Major 7" )
  val hexDorian       = Scale(Vec(0,2,3,5,7,10), 12, name = "Hex Dorian"  )
  val hexPhrygian     = Scale(Vec(0,1,3,5,8,10), 12, name = "Hex Phrygian")
  val hexSus          = Scale(Vec(0,2,5,7,9,10), 12, name = "Hex Sus"     )
  val hexMajor6       = Scale(Vec(0,2,4,5,7, 9), 12, name = "Hex Major 6" )
  val hexAeolian      = Scale(Vec(0,3,5,7,8,10), 12, name = "Hex Aeolian" )

  def default: Scale = major

  // 7 note scales
  val major           = Scale(Vec(0,2,4,5,7,9,11), 12, name = "Major"         )
  val ionian          = Scale(Vec(0,2,4,5,7,9,11), 12, name = "Ionian"        )
  val dorian          = Scale(Vec(0,2,3,5,7,9,10), 12, name = "Dorian"        )
  val phrygian        = Scale(Vec(0,1,3,5,7,8,10), 12, name = "Phrygian"      )
  val lydian          = Scale(Vec(0,2,4,6,7,9,11), 12, name = "Lydian"        )
  val mixolydian      = Scale(Vec(0,2,4,5,7,9,10), 12, name = "Mixolydian"    )
  val aeolian         = Scale(Vec(0,2,3,5,7,8,10), 12, name = "Aeolian"       )
  val minor           = Scale(Vec(0,2,3,5,7,8,10), 12, name = "Natural Minor" )
  val locrian         = Scale(Vec(0,1,3,5,6,8,10), 12, name = "Locrian"       )

  val harmonicMinor   = Scale(Vec(0,2,3,5,7,8,11), 12, name = "Harmonic Minor")
  val harmonicMajor   = Scale(Vec(0,2,4,5,7,8,11), 12, name = "Harmonic Major")

  val melodicMinor    = Scale(Vec(0,2,3,5,7,9,11), 12, name = "Melodic Minor"           )
  val melodicMinorDesc= Scale(Vec(0,2,3,5,7,8,10), 12, name = "Melodic Minor Descending")
  val melodicMajor    = Scale(Vec(0,2,4,5,7,8,10), 12, name = "Melodic Major"           )

  val bartok          = Scale(Vec(0,2,4,5,7,8,10), 12, name = "Bartok")
  val hindu           = Scale(Vec(0,2,4,5,7,8,10), 12, name = "Hindu" )

  // raga modes
  val todi            = Scale(Vec(0,1,3,6,7,8,11), 12, name = "Todi"        )
  val purvi           = Scale(Vec(0,1,4,6,7,8,11), 12, name = "Purvi"       )
  val marva           = Scale(Vec(0,1,4,6,7,9,11), 12, name = "Marva"       )
  val bhairav         = Scale(Vec(0,1,4,5,7,8,11), 12, name = "Bhairav"     )
  val ahirbhairav     = Scale(Vec(0,1,4,5,7,9,10), 12, name = "Ahirbhairav" )

  val superLocrian    = Scale(Vec(0,1,3,4,6, 8,10), 12, name = "Super Locrian"    )
  val romanianMinor   = Scale(Vec(0,2,3,6,7, 9,10), 12, name = "Romanian Minor"   )
  val hungarianMinor  = Scale(Vec(0,2,3,6,7, 8,11), 12, name = "Hungarian Minor"  )
  val neapolitanMinor = Scale(Vec(0,1,3,5,7, 8,11), 12, name = "Neapolitan Minor" )
  val enigmatic       = Scale(Vec(0,1,4,6,8,10,11), 12, name = "Enigmatic"        )
  val spanish         = Scale(Vec(0,1,4,5,7, 8,10), 12, name = "Spanish"          )

  // modes of whole tones with added note =
  val leadingWhole    = Scale(Vec(0,2,4,6,8,10,11), 12, name = "Leading Whole Tone" )
  val lydianMinor     = Scale(Vec(0,2,4,6,7, 8,10), 12, name = "Lydian Minor"       )
  val neapolitanMajor = Scale(Vec(0,1,3,5,7, 9,11), 12, name = "Neapolitan Major"   )
  val locrianMajor    = Scale(Vec(0,2,4,5,6, 8,10), 12, name = "Locrian Major"      )

  // 8 note scales
  val diminished      = Scale(Vec(0,1,3,4,6,7,9,10), 12, name = "Diminished"  )
  val diminished2     = Scale(Vec(0,2,3,5,6,8,9,11), 12, name = "Diminished 2")

  // 12 note scales
  val chromatic       = Scale(0 to 11, 12, name = "Chromatic")

  // TWENTY-FOUR TONES PER OCTAVE

  val chromatic24     = Scale(0 to 23, 24, name = "Chromatic 24")

  // maqam ajam
  val ajam            = Scale(Vec(0,4,8,10,14,18,22), 24, name = "Ajam"       )
  val jiharkah        = Scale(Vec(0,4,8,10,14,18,21), 24, name = "Jiharkah"   )
  val shawqAfza       = Scale(Vec(0,4,8,10,14,16,22), 24, name = "Shawq Afza" )

  // maqam sikah
  val sikah           = Scale(Vec(0,3,7,11,14,17,21), 24, name = "Sikah"            )
  val sikahDesc       = Scale(Vec(0,3,7,11,13,17,21), 24, name = "Sikah Descending" )
  val huzam           = Scale(Vec(0,3,7, 9,15,17,21), 24, name = "Huzam"            )
  val iraq            = Scale(Vec(0,3,7,10,13,17,21), 24, name = "Iraq"             )
  val bastanikar      = Scale(Vec(0,3,7,10,13,15,21), 24, name = "Bastanikar"       )
  val mustar          = Scale(Vec(0,5,7,11,13,17,21), 24, name = "Mustar"           )

  // maqam bayati
  val bayati          = Scale(Vec(0,3,6,10,14,16,20), 24, name = "Bayati"   )
  val karjighar       = Scale(Vec(0,3,6,10,12,18,20), 24, name = "Karjighar")
  val husseini        = Scale(Vec(0,3,6,10,14,17,21), 24, name = "Husseini" )

  // maqam nahawand
  val nahawand        = Scale(Vec(0,4,6,10,14,16,22), 24, name = "Nahawand"           )
  val nahawandDesc    = Scale(Vec(0,4,6,10,14,16,20), 24, name = "Nahawand Descending")
  val farahfaza       = Scale(Vec(0,4,6,10,14,16,20), 24, name = "Farahfaza"          )
  val murassah        = Scale(Vec(0,4,6,10,12,18,20), 24, name = "Murassah"           )
  val ushaqMashri     = Scale(Vec(0,4,6,10,14,17,21), 24, name = "Ushaq Mashri"       )

  // maqam rast
  val rast            = Scale(Vec(0,4,7,10,14,18,21), 24, name = "Rast"             )
  val rastDesc        = Scale(Vec(0,4,7,10,14,18,20), 24, name = "Rast Descending"  )
  val suznak          = Scale(Vec(0,4,7,10,14,16,22), 24, name = "Suznak"           )
  val nairuz          = Scale(Vec(0,4,7,10,14,17,20), 24, name = "Nairuz"           )
  val yakah           = Scale(Vec(0,4,7,10,14,18,21), 24, name = "Yakah"            )
  val yakahDesc       = Scale(Vec(0,4,7,10,14,18,20), 24, name = "Yakah Descending" )
  val mahur           = Scale(Vec(0,4,7,10,14,18,22), 24, name = "Mahur"            )

  // maqam hijaz
  val hijaz           = Scale(Vec(0,2,8,10,14,17,20), 24, name = "Hijaz"            )
  val hijazDesc       = Scale(Vec(0,2,8,10,14,16,20), 24, name = "Hijaz Descending" )
  val zanjaran        = Scale(Vec(0,2,8,10,14,18,20), 24, name = "Zanjaran"         )

  // maqam hijazKar
  val hijazKar        = Scale(Vec(0,2,8,10,14,16,22), 24, name = "hijazKar")

  // maqam saba
  val saba            = Scale(Vec(0,3,6,8,12,16,20), 24, name = "Saba"  )
  val zamzam          = Scale(Vec(0,2,6,8,14,16,20), 24, name = "Zamzam")

  // maqam kurd
  val kurd            = Scale(Vec(0,2,6,10,14,16,20), 24, name = "Kurd"           )
  val kijazKarKurd    = Scale(Vec(0,2,8,10,14,16,22), 24, name = "Kijaz Kar Kurd" )

  // maqam nawa Athar
  val nawaAthar       = Scale(Vec(0,4,6,12,14,16,22), 24, name = "Nawa Athar" )
  val nikriz          = Scale(Vec(0,4,6,12,14,18,20), 24, name = "Nikriz"     )
  val atharKurd       = Scale(Vec(0,2,6,12,14,16,22), 24, name = "Athar Kurd" )

// XXX TODO
//  // Ascending/descending scales
//  val melodicMinor  = ScaleAD(Vec(0,2,3, 5, 7, 9,11), 12, Vec(0,2,3, 5, 7, 8,10), name = "Melodic Minor")
//  val sikah         = ScaleAD(Vec(0,3,7,11,14,17,21), 24, Vec(0,3,7,11,13,17,21), name = "Sikah"        )
//  val nahawand      = ScaleAD(Vec(0,4,6,10,14,16,22), 24, Vec(0,4,6,10,14,16,20), name = "Nahawand"     )
}
final case class Scale private (degrees: Vec[Int], stepsPerOctave: Int, tuning: Tuning, name: String) {
  def degreeToKey(degree: Double): Double = {
    val scaleDegree = degree.round.toInt
    val accidental  = (degree - scaleDegree) * 10.0
    val baseKey     = (stepsPerOctave * (scaleDegree / size)) + wrapAt(scaleDegree)
    if (accidental == 0) baseKey else baseKey + (accidental * (stepsPerOctave / 12.0))
  }

  def size: Int = degrees.size

  def wrapAt(index: Int): Double = {
    import IntFunctions.wrap
    val indexW = wrap(index, 0, size - 1)
    tuning.wrapAt(degrees(indexW))
  }

//  def wrapAt(index: Double): Double = {
//    import DoubleFunctions.wrap
//    val indexW = wrap(index, 0, size - 1)
//    tuning.wrapAt(degrees(indexW))
//  }

  def octaveRatio: Double = tuning.octaveRatio
}
