/*
 *  LinLin.scala
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

package de.sciss.patterns.graph

import de.sciss.patterns.Types.{Bridge, NumFrac, Top}
import de.sciss.patterns.{Context, Pat, Pattern, Stream}

final case class LinLin[T1 <: Top, T2 <: Top, T <: Top](in: Pat[T1], inLo: Pat[T1], inHi: Pat[T1],
                                                        outLo: Pat[T2], outHi: Pat[T2])
                                                       (implicit br: Bridge[T1, T2, T], num: NumFrac[T])
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Stream[T#Out] = new Stream[T#Out] {
    private[this] val inStream    = in    .expand.map(br.lift1)
    private[this] val inLoStream  = inLo  .expand.map(br.lift1)
    private[this] val inHiStream  = inHi  .expand.map(br.lift1)
    private[this] val outLoStream = outLo .expand.map(br.lift2)
    private[this] val outHiStream = outHi .expand.map(br.lift2)

    def reset(): Unit = ()

    def hasNext: Boolean = inStream.hasNext && inLoStream.hasNext && inHiStream.hasNext &&
      outLoStream.hasNext && outHiStream.hasNext

    def next(): T#Out = {
      if (!hasNext) Stream.exhausted()
      val inVal     = inStream    .next()
      val inLoVal   = inLoStream  .next()
      val inHiVal   = inHiStream  .next()
      val outLoVal  = outLoStream .next()
      val outHiVal  = outHiStream .next()

      // (inVal - inLoVal) / (inHiVal - inLoVal) * (outHiVal - outLoVal) + outLoVal
      num.plus(num.times(num.div(num.minus(inVal, inLoVal), num.minus(inHiVal, inLoVal)),
        num.minus(outHiVal, outLoVal)), outLoVal)
    }
  }
}
