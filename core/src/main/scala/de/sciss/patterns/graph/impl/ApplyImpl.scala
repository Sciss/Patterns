/*
 *  ApplyImpl.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object ApplyImpl extends StreamFactory {
  final val typeId = 0x4170706C // "Appl"

  def expand[S <: Base[S], A](pat: Apply[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val idxStream   = idx .expand[S]
    val state       = tx.newVar[Stream[S, A]](id, null)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, idxStream = idxStream,
      valid = valid, _hasNext = _hasNext, state = state)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Pat[A]](in, access)
    val idxStream   = Stream.read[S, Int   ](in, access)
    val state       = tx.newVar[Stream[S, A]](id, null)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, idxStream = idxStream,
      valid = valid, _hasNext = _hasNext, state = state)
  }

  private final class StreamImpl[S <: Base[S], A](
    id        : S#Id,
    inStream  : Stream[S, Pat[A]],
    idxStream : Stream[S, Int],
    state     : S#Var[Stream[S, A]],
    _hasNext  : S#Var[Boolean],
    valid     : S#Var[Boolean]
  )
    extends Stream[S, A] {

    protected def typeId: Int = ApplyImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id       .write(out)
      inStream .write(out)
      idxStream.write(out)
      state    .write(out)
      _hasNext .write(out)
      valid    .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id       .dispose()
      inStream .dispose()
      idxStream.dispose()
      state    .dispose()
      _hasNext .dispose()
      valid    .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid()) {
      valid() = true
      val xhn = idxStream.hasNext
      if (xhn) {
        val idxVal = idxStream.next()
        if (idxVal >= 0) {
          var i = 0
          while (i < idxVal && inStream.hasNext) {
            inStream.next()
            i += 1
          }
          if (inStream.hasNext) {
            val inVal   = inStream.next()
            val _state  = inVal.expand[S]
            state()     = _state
            _hasNext()  = _state.hasNext
          }

        } else {
          _hasNext() = false
        }

      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val _state  = state()
      val res     = _state.next()
      if (!_state.hasNext) _hasNext() = false
      res
    }
  }
}
