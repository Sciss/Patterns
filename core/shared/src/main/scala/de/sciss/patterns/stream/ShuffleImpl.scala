/*
 *  ShuffleImpl.scala
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

import de.sciss.lucre.{Exec, Ident, RandomObj, Var}
import de.sciss.patterns.graph.Shuffle
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object ShuffleImpl extends StreamFactory {
  final val typeId = 0x53687566 // "Shuf"

  def expand[T <: Exec[T], A](pat: Shuffle[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand(ctx, tx)
    val count     = id.newIntVar(0)
    val shuffled  = id.newVar[Vec[A]](Vector.empty)(tx, PatElem.vecFormat)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)
    val r         = ctx.mkRandom(ref)
    
    new StreamImpl[T, A](id = id, inStream = inStream, count = count, shuffled = shuffled,
      _hasNext = _hasNext, valid = valid)(r)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val count     = id.readIntVar(in)
    val shuffled  = id.readVar[Vec[Any]](in)(PatElem.vecFormat)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)
    val r         = RandomObj.read[T](in)

    new StreamImpl[T, Any](id = id, inStream = inStream, count = count, shuffled = shuffled,
      _hasNext = _hasNext, valid = valid)(r)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   count   : Var[T, Int],
                                                   shuffled: Var[T, Vec[A]],
                                                   _hasNext: Var[T, Boolean],
                                                   valid   : Var[T, Boolean]
  ) (
    implicit r: RandomObj[T]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val countOut    = idOut.newIntVar(count())
      val shuffledOut = idOut.newVar[Vec[A]](shuffled())(txOut, PatElem.vecFormat)
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())
      val rOut        = r.copy[Out]()

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, count = countOut, shuffled = shuffledOut,
        _hasNext = hasNextOut, valid = validOut)(rOut)
    }

    protected def typeId: Int = ShuffleImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      count   .write(out)
      shuffled.write(out)
      _hasNext.write(out)
      valid   .write(out)
      r       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      count   .dispose()
      shuffled.dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
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

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
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
