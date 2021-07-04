/*
 *  GeomSeqImpl.scala
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

import de.sciss.lucre.{Adjunct, Exec, Ident, Var}
import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.patterns.graph.GeomSeq
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.stream.impl.SeriesLikeStreamImpl
import de.sciss.serial.DataInput

object GeomSeqImpl extends StreamFactory {
  final val typeId = 0x47656F6D // "Geom"

  def expand[T <: Exec[T], A1, A2, A](pat: GeomSeq[A1, A2, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._

    val id          = tx.newId()
    val startStream = start .expand[T]
    val stepStream  = factor  .expand[T]
    val state       = PatElem.makeVar[T, A](id)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A1, A2, A](id = id, startStream = startStream, stepStream = stepStream, state = state,
      _hasNext = _hasNext, valid = valid)(num, widen)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val startStream = Stream.read[T, Any](in)
    val stepStream  = Stream.read[T, Any](in)
    val state       = PatElem.readVar[T, Any](id, in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)
    val num         = Adjunct.readT[Num[Any]](in)
    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)

    new StreamImpl[T, Any, Any, Any](id = id, startStream = startStream, stepStream = stepStream, state = state,
      _hasNext = _hasNext, valid = valid)(num, widen)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2, A](
                                                           protected val id          : Ident[T],
                                                           protected val startStream : Stream[T, A1],
                                                           protected val stepStream  : Stream[T, A2],
                                                           protected val state       : Var[T, A],
                                                           protected val _hasNext    : Var[T, Boolean],
                                                           protected val valid       : Var[T, Boolean]
  )(
    implicit protected val num: Num[A],
    protected val widen: Widen2[A1, A2, A]
  )
    extends SeriesLikeStreamImpl[T, A1, A2, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val startStreamOut  = c(startStream)
      val stepStreamOut   = c(stepStream )
      val stateOut        = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A1, A2, A](id = idOut, startStream = startStreamOut, stepStream = stepStreamOut, state = stateOut,
        _hasNext = hasNextOut, valid = validOut)(num, widen)
    }

    protected def typeId: Int = GeomSeqImpl.typeId

    protected def op(a: A, b: A): A = num.times(a, b)
  }
}
