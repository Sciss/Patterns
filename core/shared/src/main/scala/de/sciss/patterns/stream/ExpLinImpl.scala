/*
 *  ExpLinImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.Adjunct
import de.sciss.lucre.Adjunct.{NumDouble, Widen2}
import de.sciss.lucre.Exec
import de.sciss.patterns.graph.ExpLin
import de.sciss.patterns.stream.impl.ScaleLikeStreamImpl
import de.sciss.serial.DataInput

object ExpLinImpl extends StreamFactory {
  final val typeId = 0x45787045 // "ExpL"

  def expand[T <: Exec[T], A1, A2, A](pat: ExpLin[A1, A2, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val inStream    = in    .expand[T]
    val inLoStream  = inLo  .expand[T]
    val inHiStream  = inHi  .expand[T]
    val outLoStream = outLo .expand[T]
    val outHiStream = outHi .expand[T]

    new StreamImpl[T, A1, A2, A](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val inStream    = Stream.read[T, Any](in)
    val inLoStream  = Stream.read[T, Any](in)
    val inHiStream  = Stream.read[T, Any](in)
    val outLoStream = Stream.read[T, Any](in)
    val outHiStream = Stream.read[T, Any](in)

    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)
    val num         = Adjunct.readT[NumDouble[Any]]       (in)

    new StreamImpl[T, Any, Any, Any](inStream = inStream, inLoStream = inLoStream, inHiStream = inHiStream,
      outLoStream = outLoStream, outHiStream = outHiStream)(widen, num)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2, A](
                                                           protected val inStream   : Stream[T, A1],
                                                           protected val inLoStream : Stream[T, A1],
                                                           protected val inHiStream : Stream[T, A1],
                                                           protected val outLoStream: Stream[T, A2],
                                                           protected val outHiStream: Stream[T, A2]
                                                         )(
                                                           implicit protected val widen: Widen2[A1, A2, A],
                                                           protected val num: NumDouble[A]
                                                         )
    extends ScaleLikeStreamImpl[T, A1, A2, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
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
