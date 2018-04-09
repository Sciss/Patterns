/*
 *  ShuffleImpl.scala
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

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.graph.Shuffle
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object ShuffleImpl extends StreamFactory {
  final val typeId = 0x53687566 // "Shuf"

  def expand[S <: Base[S], A](pat: Shuffle[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand(ctx, tx)
    val count     = tx.newIntVar(id, 0)
    val shuffled  = tx.newVar[Vec[A]](id, Vector.empty)(PatElem.vecSerializer)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
    val r         = ctx.mkRandom(ref)
    
    new StreamImpl[S, A](id = id, inStream = inStream, count = count, shuffled = shuffled, 
      _hasNext = _hasNext, valid = valid)(r)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, A](in, access)
    val count     = tx.readIntVar(id, in)
    val shuffled  = tx.readVar[Vec[A]](id, in)(PatElem.vecSerializer)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val r         = TxnRandom.read[S](in, access)

    new StreamImpl[S, A](id = id, inStream = inStream, count = count, shuffled = shuffled,
      _hasNext = _hasNext, valid = valid)(r)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   count   : S#Var[Int],
                                                   shuffled: S#Var[Vec[A]],
                                                   _hasNext: S#Var[Boolean],
                                                   valid   : S#Var[Boolean]
  ) (
    implicit r: TxnRandom[S]
  )
    extends Stream[S, A] {

    protected def typeId: Int = ShuffleImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      count   .write(out)
      shuffled.write(out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      count   .dispose()
      shuffled.dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      count()   = 0
      var rem   = inStream.toVector
      val b     = Vector.newBuilder[A]
      b.sizeHint(rem.size)
      // XXX TODO --- this is probably not the fastest possible implementation
      while (rem.nonEmpty) {
        val idx = r.nextInt(rem.size)
        val e   = rem(idx)
        rem     = rem.patch(idx, Nil, 1)
        b      += e
      }
      val vec     = b.result()
      shuffled()  = vec
      _hasNext()  = vec.nonEmpty
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val vec = shuffled()
      val c   = count()
      val res = vec(c)
      val c1  = c + 1
      count() = c1
      if (c1 == vec.size) _hasNext() = false
      res
    }
  }
}
