/*
 *  ApplyImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Apply, Pat}
import de.sciss.serial.{DataInput, DataOutput}

object ApplyImpl extends StreamFactory {
  final val typeId = 0x4170706C // "Appl"

  def expand[T <: Exec[T], A](pat: Apply[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val idxStream   = idx .expand[T]
    val state       = id.newVar[Stream[T, A]](null)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, idxStream = idxStream,
      valid = valid, _hasNext = _hasNext, state = state)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Pat[Any]](in)
    val idxStream   = Stream.read[T, Int   ](in)
    val state       = id.readVar[Stream[T, Any]](in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, idxStream = idxStream,
      valid = valid, _hasNext = _hasNext, state = state)
  }

  private final class StreamImpl[T <: Exec[T], A](id        : Ident[T],
                                                  inStream  : Stream[T, Pat[A]],
                                                  idxStream : Stream[T, Int],
                                                  state     : Var[T, Stream[T, A]],
                                                  _hasNext  : Var[T, Boolean],
                                                  valid     : Var[T, Boolean]
  )
    extends Stream[T, A] {

    private[patterns] override def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                               (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val inStreamOut   = c(inStream)
      val idxStreamOut  = c(idxStream)
      val stateOut      = c.copyVar(idOut, state)
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, idxStream = idxStreamOut,
        valid = validOut, _hasNext = hasNextOut, state = stateOut)
    }

    protected def typeId: Int = ApplyImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id       .write(out)
      inStream .write(out)
      idxStream.write(out)
      state    .write(out)
      _hasNext .write(out)
      valid    .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id       .dispose()
      inStream .dispose()
      idxStream.dispose()
      state    .dispose()
      _hasNext .dispose()
      valid    .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
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
            val _state  = inVal.expand[T]
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

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val _state  = state()
      val res     = _state.next()
      if (!_state.hasNext) _hasNext() = false
      res
    }
  }
}
