/*
 *  LinLinImpl.scala
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
package stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns
import de.sciss.patterns.Types.{Aux, NumFrac, Widen2}
import de.sciss.patterns.graph.LinLin
import de.sciss.patterns.stream.impl.ScaleLikeStreamImpl
import de.sciss.serial.DataInput

object LinLinImpl extends StreamFactory {
  final val typeId = 0x4C696E4C // "LinL"

  def expand[S <: Base[S], A1, A2, A](pat: LinLin[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A] = {
    import pat._
    val inStream    = in    .expand[S]
    val inLoStream  = inLo  .expand[S]
    val inHiStream  = inHi  .expand[S]
    val outLoStream = outLo .expand[S]
    val outHiStream = outHi .expand[S]

    new StreamImpl[S, A1, A2, A](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val inStream    = patterns.Stream.read[S, Any](in, access)
    val inLoStream  = patterns.Stream.read[S, Any](in, access)
    val inHiStream  = patterns.Stream.read[S, Any](in, access)
    val outLoStream = patterns.Stream.read[S, Any](in, access)
    val outHiStream = patterns.Stream.read[S, Any](in, access)

    val widen       = Aux.readT[Widen2[Any, Any, A]](in)
    val num         = Aux.readT[NumFrac[A]]         (in)

    new StreamImpl[S, Any, Any, A](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
                                                           protected val inStream   : patterns.Stream[S, A1],
                                                           protected val inLoStream : patterns.Stream[S, A1],
                                                           protected val inHiStream : patterns.Stream[S, A1],
                                                           protected val outLoStream: patterns.Stream[S, A2],
                                                           protected val outHiStream: patterns.Stream[S, A2]
                                                         )(
                                                           implicit protected val widen: Widen2[A1, A2, A],
                                                           protected val num: NumFrac[A]
                                                         )
    extends ScaleLikeStreamImpl[S, A1, A2, A] {

    protected def typeId: Int = LinLinImpl.typeId

    // (in - inLow) / (inHigh - inLow) * (outHigh - outLow) + outLow
    protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A =
      num.+(num.*(num./(num.-(inVal, inLoVal), num.-(inHiVal, inLoVal)),
        num.-(outHiVal, outLoVal)), outLoVal)
  }
}
