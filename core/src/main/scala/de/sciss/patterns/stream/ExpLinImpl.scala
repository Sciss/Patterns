/*
 *  ExpLinImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.adjunct.Adjunct.{NumDouble, Widen2}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.ExpLin
import de.sciss.patterns.stream.impl.ScaleLikeStreamImpl
import de.sciss.serial.DataInput

object ExpLinImpl extends StreamFactory {
  final val typeId = 0x45787045 // "ExpL"

  def expand[S <: Base[S], A1, A2, A](pat: ExpLin[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val inStream    = in    .expand[S]
    val inLoStream  = inLo  .expand[S]
    val inHiStream  = inHi  .expand[S]
    val outLoStream = outLo .expand[S]
    val outHiStream = outHi .expand[S]

    new StreamImpl[S, A1, A2, A](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val inStream    = Stream.read[S, Any](in, access)
    val inLoStream  = Stream.read[S, Any](in, access)
    val inHiStream  = Stream.read[S, Any](in, access)
    val outLoStream = Stream.read[S, Any](in, access)
    val outHiStream = Stream.read[S, Any](in, access)

    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)
    val num         = Adjunct.readT[NumDouble[Any]]       (in)

    new StreamImpl[S, Any, Any, Any](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
                                                           protected val inStream   : Stream[S, A1],
                                                           protected val inLoStream : Stream[S, A1],
                                                           protected val inHiStream : Stream[S, A1],
                                                           protected val outLoStream: Stream[S, A2],
                                                           protected val outHiStream: Stream[S, A2]
                                                         )(
                                                           implicit protected val widen: Widen2[A1, A2, A],
                                                           protected val num: NumDouble[A]
                                                         )
    extends ScaleLikeStreamImpl[S, A1, A2, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val inStreamOut    = c(inStream   )
      val inLoStreamOut  = c(inLoStream )
      val inHiStreamOut  = c(inHiStream )
      val outLoStreamOut = c(outLoStream)
      val outHiStreamOut = c(outHiStream)

      new StreamImpl[Out, A1, A2, A](inStream = inStreamOut, inLoStream = inLoStreamOut, inHiStream = inHiStreamOut,
        outLoStream = outLoStreamOut, outHiStream = outHiStreamOut)(widen, num)
    }

    protected def typeId: Int = ExpLinImpl.typeId

    // math.log(in / inLow) / math.log(inHigh / inLow) * (outHigh - outLow) + outLow
    protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A =
      num.+(
        num.*(
          num./(
            num.log(num./(inVal  , inLoVal)),
            num.log(num./(inHiVal, inLoVal))),
          num.-(outHiVal, outLoVal)
        ),
        outLoVal
      )
  }
}
