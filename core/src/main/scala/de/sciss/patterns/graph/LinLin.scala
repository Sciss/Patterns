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

import de.sciss.patterns.Types.{Aux, NumFrac, Widen2}
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}

final case class LinLin[A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                   outLo: Pat[A2], outHi: Pat[A2])
                                  (implicit w: Widen2[A1, A2, A], num: NumFrac[A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = w :: num :: Nil

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT     = t(in)
    val inLoT   = t(inLo)
    val inHiT   = t(inHi)
    val outLoT  = t(outLo)
    val outHiT  = t(outHi)
    if (inT.eq(in) && inLoT.eq(inLo) && inHiT.eq(inHi) && outLoT.eq(outLo) && outHiT.eq(outHi)) this
    else copy(in = inT, inLo = inLoT, inHi = inHiT, outLo = outLoT, outHi = outHiT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream    = in    .expand(ctx, tx0).map(w.widen1)
    private[this] val inLoStream  = inLo  .expand(ctx, tx0).map(w.widen1)
    private[this] val inHiStream  = inHi  .expand(ctx, tx0).map(w.widen1)
    private[this] val outLoStream = outLo .expand(ctx, tx0).map(w.widen2)
    private[this] val outHiStream = outHi .expand(ctx, tx0).map(w.widen2)

    def reset()(implicit tx: Tx): Unit = {
      inStream    .reset()
      inLoStream  .reset()
      inHiStream  .reset()
      outLoStream .reset()
      outHiStream .reset()
    }

    def hasNext(implicit tx: Tx): Boolean =
      inStream   .hasNext &&
      inLoStream .hasNext &&
      inHiStream .hasNext &&
      outLoStream.hasNext &&
      outHiStream.hasNext

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
