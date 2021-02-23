/*
 *  UpdatedAllImpl.scala
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
import de.sciss.patterns.graph.UpdatedAll
import de.sciss.serial.{DataInput, DataOutput}

object UpdatedAllImpl extends StreamFactory {
  final val typeId = 0x5570416C // "UpAl"

  def expand[T <: Exec[T], A1, A >: A1](pat: UpdatedAll[A1, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand(ctx, tx)
    val idxStream   = idx .expand(ctx, tx)
    val elemStream  = elem.expand(ctx, tx)
    val state       = id.newVar[Stream[T, A]](null)
    val valid       = id.newBooleanVar(false)
    
    new StreamImpl[T, A1, A](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
      state = state, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any](in)
    val idxStream   = Stream.read[T, Int](in)
    val elemStream  = Stream.read[T, Any](in)
    val state       = id.readVar[Stream[T, Any]](in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any, Any](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
      state = state, valid = valid)
  }


  private final class StreamImpl[T <: Exec[T], A1, A >: A1](
                                                             id        : Ident[T],
                                                             inStream  : Stream[T, A1],
                                                             idxStream : Stream[T, Int],
                                                             elemStream: Stream[T, A],
                                                             state     : Var[T, Stream[T, A]],
                                                             valid     : Var[T, Boolean]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut          = txOut.newId()
      val inStreamOut    = c(inStream  )
      val idxStreamOut   = c(idxStream )
      val elemStreamOut  = c(elemStream)
      val stateOut       = c.copyVar(idOut, state)
      val validOut       = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A1, A](id = idOut, inStream = inStreamOut, idxStream = idxStreamOut, elemStream = elemStreamOut,
        state = stateOut, valid = validOut)
    }

    protected def typeId: Int = UpdatedAllImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      idxStream .write(out)
      elemStream.write(out)
      state     .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      inStream  .dispose()
      idxStream .dispose()
      elemStream.dispose()
      state     .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      idxStream .reset()
      elemStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      //      DEBUG += 1
      //      if (DEBUG == 7106) {
      //        println("BOO")
      //      }
      //      if (DEBUG == 7107) {
      //        println("AQUI")
      //      }
      var vec = inStream.toVector: Vector[A]
      while (idxStream.hasNext && elemStream.hasNext) {
        val idxVal  = idxStream .next()
        val elemVal = elemStream.next()
        //        if (idxVal > vec.size) {
        //          println("Potzblitz")
        //        }
        vec         = vec.updated(idxVal, elemVal)
      }
      val _state  = Stream(vec: _*)
      state()     = _state
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      state().hasNext
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      state().next()
    }
  }
}
