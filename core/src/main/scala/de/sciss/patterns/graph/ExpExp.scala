/*
 *  ExpExp.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, NumDouble, Widen2}
import de.sciss.patterns.graph.impl.ScaleLikeStream
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}
import de.sciss.serial.DataOutput

final case class ExpExp[A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                   outLo: Pat[A2], outHi: Pat[A2])
                                  (implicit w: Widen2[A1, A2, A], num: NumDouble[A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = w :: num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT     = t(in)
    val inLoT   = t(inLo)
    val inHiT   = t(inHi)
    val outLoT  = t(outLo)
    val outHiT  = t(outHi)
    if (inT.eq(in) && inLoT.eq(inLo) && inHiT.eq(inHi) && outLoT.eq(outLo) && outHiT.eq(outHi)) this
    else copy(in = inT, inLo = inLoT, inHi = inHiT, outLo = outLoT, outHi = outHiT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends ScaleLikeStream(in = in, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi, tx0 = tx0) {

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    // math.pow(outHigh / outLow, math.log(in / inLow) / math.log(inHigh / inLow)) * outLow
    protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A =
      num.*(
        num.pow(
          num./(outHiVal, outLoVal),
          num./(
            num.log(num./(inVal  , inLoVal)),
            num.log(num./(inHiVal, inLoVal))
          )
        ),
        outLoVal
      )
  }
}
