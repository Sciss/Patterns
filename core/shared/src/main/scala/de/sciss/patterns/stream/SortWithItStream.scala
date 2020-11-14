/*
 *  SortWithItStream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object SortWithItStream extends StreamFactory {
  final val typeId = 0x53574974 // "SWIt"

  def expand[T <: Exec[T], A](token: Int)(implicit ctx: Context[T], tx: T): SortWithItStream[T, A] = {
    val id        = tx.newId()
    val pairIn    = {
      implicit val vec: ConstFormat[Vec[A]] = PatElem.vecFormat[A]
      id.newVar[(Vec[A], Vec[A])]((Vector.empty, Vector.empty))
    }
    val count     = id.newIntVar(0)
    val hasZ      = id.newBooleanVar(false)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new Impl[T, A](id = id, token = token, pairIn = pairIn, count = count, hasZ = hasZ,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val token     = in.readInt()
    val pairIn    = {
      implicit val vec: ConstFormat[Vec[Any]] = PatElem.vecFormat[Any]
      id.readVar[(Vec[Any], Vec[Any])](in)
    }
    val count     = id.readIntVar(in)
    val hasZ      = id.readBooleanVar(in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)

    val res = new Impl[T, Any](id = id, token = token, pairIn = pairIn, count = count, hasZ = hasZ,
      _hasNext = _hasNext, valid = valid)
    ctx.registerItStream(res)
    res
  }


  private final class Impl[T <: Exec[T], A](
                                            id        : Ident[T],
                                            val token : Int,
                                            pairIn    : Var[T, (Vec[A], Vec[A])],
                                            count     : Var[T, Int],
                                            hasZ      : Var[T, Boolean],
                                            _hasNext  : Var[T, Boolean],
                                            valid     : Var[T, Boolean]
                                           )
    extends SortWithItStream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, (A, A)] = {
      val idOut     = txOut.newId()
      val pairInOut = {
        implicit val vec: ConstFormat[Vec[A]] = PatElem.vecFormat[A]
        idOut.newVar[(Vec[A], Vec[A])](pairIn())
      }
      val countOut    = idOut.newIntVar    (count())
      val hasZOut     = idOut.newBooleanVar(hasZ())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new Impl[Out, A](id = idOut, token = token, pairIn = pairInOut, count = countOut, hasZ = hasZOut,
        _hasNext = hasNextOut, valid = validOut)
    }

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

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      pairIn  .dispose()
      count   .dispose()
      hasZ    .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def advance(x: Vec[A], y: Vec[A])(implicit tx: T): Unit = {
      pairIn() = (x, y)
      count()     = 0
      hasZ()     = true
      calcHasNext()
    }

    private def calcHasNext()(implicit tx: T): Unit = {
      if (hasZ()) {
        val (x, y) = pairIn()
        val sz      = math.min(x.size, y.size)
        val hn      = sz > count()
        _hasNext()  = hn
      } else {
        _hasNext() = false
      }
    }

    private def validate()(implicit tx: T): Unit =
      if (!valid()) {
        valid()    = true
  //      _hasZ()     = false
        count()     = 0
        calcHasNext()
      }

  //  def resetOuter()(implicit tx: T): Unit = {
  //    _valid() = false
  //  }

    def reset()(implicit tx: T): Unit =
      valid() = false

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      hasZ() && _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): (A, A) = {
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
trait SortWithItStream[T <: Exec[T], A] extends ItStream[T, (A, A)] {
  def advance(x: Vec[A], y: Vec[A])(implicit tx: T): Unit
}