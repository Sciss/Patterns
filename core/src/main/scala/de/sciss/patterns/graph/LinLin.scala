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

import de.sciss.patterns.Types.{Aux, Bridge, NumFrac, Top}
import de.sciss.patterns.{Context, Pat, Pattern, Stream}

final case class LinLin[T1 <: Top, T2 <: Top, T <: Top](in: Pat[T1], inLo: Pat[T1], inHi: Pat[T1],
                                                        outLo: Pat[T2], outHi: Pat[T2])
                                                       (implicit br: Bridge[T1, T2, T], num: NumFrac[T])
  extends Pattern[T] {

  override private[patterns] def aux: List[Aux] = br :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = new Stream[Tx, T#Out[Tx]] {
    private[this] val inStream    = in    .expand.map(br.lift1)
    private[this] val inLoStream  = inLo  .expand.map(br.lift1)
    private[this] val inHiStream  = inHi  .expand.map(br.lift1)
    private[this] val outLoStream = outLo .expand.map(br.lift2)
    private[this] val outHiStream = outHi .expand.map(br.lift2)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean =
      inStream   .hasNext &&
      inLoStream .hasNext && inHiStream .hasNext &&
      outLoStream.hasNext && outHiStream.hasNext

    def next()(implicit tx: Tx): T#Out[Tx] = {
      if (!hasNext) Stream.exhausted()
      val inVal     = inStream    .next()
      val inLoVal   = inLoStream  .next()
      val inHiVal   = inHiStream  .next()
      val outLoVal  = outLoStream .next()
      val outHiVal  = outHiStream .next()

      // (inVal - inLoVal) / (inHiVal - inLoVal) * (outHiVal - outLoVal) + outLoVal
      num.plus[Tx](num.times[Tx](num.div[Tx](num.minus[Tx](inVal, inLoVal), num.minus[Tx](inHiVal, inLoVal)),
        num.minus(outHiVal, outLoVal)), outLoVal)
    }
  }
}
