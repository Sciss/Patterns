/*
 *  ExpLin.scala
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

import de.sciss.patterns.Types.{Aux, NumDouble, Widen2}
import de.sciss.patterns.graph.impl.ScaleLikeStream
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}

final case class ExpLin[A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                   outLo: Pat[A2], outHi: Pat[A2])
                                  (implicit w: Widen2[A1, A2, A], num: NumDouble[A])
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

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends ScaleLikeStream(in = in, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi, tx0 = tx0) {

    // math.log(in / inLow) / math.log(inHigh / inLow) * (outHigh - outLow) + outLow
    protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A =
      num.plus(
        num.times(
          num.div(
            num.log(num.div(inVal  , inLoVal)),
            num.log(num.div(inHiVal, inLoVal))),
          num.minus(outHiVal, outLoVal)
        ),
        outLoVal
      )
  }
}
