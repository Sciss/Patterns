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

import de.sciss.patterns.Types.{Aux, NumFrac, Widen}
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}

final case class LinLin[A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                   outLo: Pat[A2], outHi: Pat[A2])
                                  (implicit widen: Widen[A1, A2, A], num: NumFrac[A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform(t: Transform): Pat[A] = ???

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream    = in    .expand(ctx, tx0).map(widen.lift1)
    private[this] val inLoStream  = inLo  .expand(ctx, tx0).map(widen.lift1)
    private[this] val inHiStream  = inHi  .expand(ctx, tx0).map(widen.lift1)
    private[this] val outLoStream = outLo .expand(ctx, tx0).map(widen.lift2)
    private[this] val outHiStream = outHi .expand(ctx, tx0).map(widen.lift2)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean =
      inStream   .hasNext &&
      inLoStream .hasNext && inHiStream .hasNext &&
      outLoStream.hasNext && outHiStream.hasNext

    def next()(implicit tx: Tx): A = {
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
