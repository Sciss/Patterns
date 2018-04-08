/*
 *  UpdatedAllImpl.scala
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

object UpdatedAllImpl extends StreamFactory {
  final val typeId = 0x5570416C // "UpAl"

  def expand[S <: Base[S], A1, A >: A1](pat: UpdatedAll[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand(ctx, tx)
    val idxStream   = idx .expand(ctx, tx)
    val elemStream  = elem.expand(ctx, tx)
    val state       = tx.newVar[Stream[S, A]](id, null)
    val valid       = tx.newBooleanVar(id, false)
    
    new StreamImpl[S, A1, A](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
      state = state, valid = valid)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, A  ](in, access)
    val idxStream   = Stream.read[S, Int](in, access)
    val elemStream  = Stream.read[S, A  ](in, access)
    val state       = tx.readVar[Stream[S, A]](id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, A, A](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
      state = state, valid = valid)
  }


  private final class StreamImpl[S <: Base[S], A1, A >: A1](
    id        : S#Id,
    inStream  : Stream[S, A1],
    idxStream : Stream[S, Int],
    elemStream: Stream[S, A],
    state     : S#Var[Stream[S, A]],
    valid     : S#Var[Boolean]
  )
    extends Stream[S, A] {

    protected def typeId: Int = UpdatedAllImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      inStream  .write(out)
      idxStream .write(out)
      elemStream.write(out)
      state     .write(out)
      valid     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      inStream  .dispose()
      idxStream .dispose()
      elemStream.dispose()
      state     .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      idxStream .reset()
      elemStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      state().hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      state().next()
    }
  }
}
