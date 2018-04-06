/*
 *  ArithmSeqImpl.scala
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
package graph
package impl

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Num, Widen2}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.DataInput

object ArithmSeqImpl extends StreamFactory {
  final val typeId = 0x41726974 // "Arit"

  def expand[S <: Base[S], A1, A2, A](pat: ArithmSeq[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._

    val id          = tx.newId()
    val startStream = start .expand[S]
    val stepStream  = step  .expand[S]
    val state       = PatElem.makeVar[S, A](id)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A1, A2, A](id = id, startStream = startStream, stepStream = stepStream, state = state,
      _hasNext = _hasNext, valid = valid)(num, widen)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id          = tx.readId(in, access)
    val startStream = Stream.read[S, Any](in, access)
    val stepStream  = Stream.read[S, Any](in, access)
    val state       = PatElem.readVar[S, A](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)
    val num         = Aux.readT[Num[A]](in)
    val widen       = Aux.readT[Widen2[Any, Any, A]](in)

    new StreamImpl[S, Any, Any, A](id = id, startStream = startStream, stepStream = stepStream, state = state,
      _hasNext = _hasNext, valid = valid)(num, widen)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
    protected val id          : S#Id,
    protected val startStream : Stream[S, A1],
    protected val stepStream  : Stream[S, A2],
    protected val state       : S#Var[A],
    protected val _hasNext    : S#Var[Boolean],
    protected val valid       : S#Var[Boolean]
  )(
    implicit protected val num: Num[A],
    protected val widen: Widen2[A1, A2, A]
  )
    extends SeriesLikeStreamImpl[S, A1, A2, A] {

    protected def typeId: Int = ArithmSeqImpl.typeId

    protected def op(a: A, b: A): A = num.+(a, b)
  }
}
