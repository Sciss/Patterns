/*
 *  SortWithItStream.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object SortWithItStream extends StreamFactory {
  final val typeId = 0x53574974 // "SWIt"

  def expand[S <: Base[S], A](token: Int)(implicit ctx: Context[S], tx: S#Tx): SortWithItStream[S, A] = {
    val id        = tx.newId()
    val pairIn    = {
      implicit val vec: ImmutableSerializer[Vec[A]] = PatElem.vecSerializer[A]
      tx.newVar[(Vec[A], Vec[A])](id, (Vector.empty, Vector.empty))
    }
    val count     = tx.newIntVar(id, 0)
    val hasZ      = tx.newBooleanVar(id, false)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new Impl[S, A](id = id, token = token, pairIn = pairIn, count = count, hasZ = hasZ,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val token     = in.readInt()
    val pairIn    = {
      implicit val vec: ImmutableSerializer[Vec[Any]] = PatElem.vecSerializer[Any]
      tx.readVar[(Vec[Any], Vec[Any])](id, in)
    }
    val count     = tx.readIntVar(id, in)
    val hasZ      = tx.readBooleanVar(id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    val res = new Impl[S, Any](id = id, token = token, pairIn = pairIn, count = count, hasZ = hasZ,
      _hasNext = _hasNext, valid = valid)
    ctx.registerItStream(res)
    res
  }


  private final class Impl[S <: Base[S], A](
                                            id        : S#Id,
                                            val token : Int,
                                            pairIn    : S#Var[(Vec[A], Vec[A])],
                                            count     : S#Var[Int],
                                            hasZ      : S#Var[Boolean],
                                            _hasNext  : S#Var[Boolean],
                                            valid     : S#Var[Boolean]
                                           )
    extends SortWithItStream[S, A] {


    //      ({
    //      implicit val vec: ImmutableSerializer[Vec[A]] = PatElem.vecSerializer[A]
    //      ImmutableSerializer.tuple2[Vec[A], Vec[A]]
    //    })

    protected def typeId: Int = SortWithItStream.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      out.writeInt(token)
      pairIn  .write(out)
      count   .write(out)
      hasZ    .write(out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      pairIn  .dispose()
      count   .dispose()
      hasZ    .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def advance(x: Vec[A], y: Vec[A])(implicit tx: S#Tx): Unit = {
      pairIn() = (x, y)
      count()     = 0
      hasZ()     = true
      calcHasNext()
    }

    private def calcHasNext()(implicit tx: S#Tx): Unit = {
      if (hasZ()) {
        val (x, y) = pairIn()
        val sz      = math.min(x.size, y.size)
        val hn      = sz > count()
        _hasNext()  = hn
      } else {
        _hasNext() = false
      }
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!valid()) {
        valid()    = true
  //      _hasZ()     = false
        count()     = 0
        calcHasNext()
      }

  //  def resetOuter()(implicit tx: S#Tx): Unit = {
  //    _valid() = false
  //  }

    def reset()(implicit tx: S#Tx): Unit =
      valid() = false

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      hasZ() && _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): (A, A) = {
      if (!hasNext) Stream.exhausted()
      val (x, y)  = pairIn()
      val sz      = math.min(x.size, y.size)
      val c0      = count()
      val res     = (x(c0), y(c0))
      val c1      = c0 + 1
      count()     = c1
      if (c1 == sz) {
        hasZ()     = false
        _hasNext()  = false
      }
      res
    }
  }
}
trait SortWithItStream[S <: Base[S], A] extends ItStream[S, (A, A)] {
  def advance(x: Vec[A], y: Vec[A])(implicit tx: S#Tx): Unit
}