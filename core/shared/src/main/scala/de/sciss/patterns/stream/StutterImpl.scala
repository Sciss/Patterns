/*
 *  StutterImpl.scala
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
import de.sciss.patterns.graph.Stutter
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object StutterImpl extends StreamFactory {
  final val typeId = 0x53747574 // "Stut"

  def expand[T <: Exec[T], A](pat: Stutter[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val nStream   = n .expand[T]
    val state     = PatElem.makeVar[T, A](id)
    val remain    = id.newIntVar(0)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, nStream = nStream, state = state, remain = remain,
      valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any  ](in)
    val nStream   = Stream.read[T, Int](in)
    val state     = PatElem.readVar[T, Any](id, in)
    val remain    = id.readIntVar(in)
    val valid     = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, nStream = nStream, state = state, remain = remain,
      valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   protected val id        : Ident[T],
                                                   protected val inStream  : Stream[T, A],
                                                   protected val nStream   : Stream[T, Int],
                                                   protected val state     : Var[T, A],
                                                   protected val remain    : Var[T, Int],
                                                   protected val valid     : Var[T, Boolean]
                                                 )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val nStreamOut  = c(nStream )
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val remainOut   = idOut.newIntVar(remain())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, nStream = nStreamOut, state = stateOut,
        remain = remainOut, valid = validOut)
    }

    protected def typeId: Int = StutterImpl.typeId

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
      nStream .reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
      val nhn = nStream.hasNext
      if (nhn) {
        val nVal = math.max(0, nStream.next())
        if (nVal > 0) {
          val ihn = inStream.hasNext
          if (ihn) {
            val inVal = inStream.next()
            state()   = inVal
            remain()  = nVal
          } else {
            remain() = 0
          }
        } else {
          advance()
        }

      } else {
        remain() = 0
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      remain() > 0
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      val n1 = remain() - 1
      remain() = n1
      if (n1 == 0) advance()
      res
    }

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      nStream .write(out)
      state   .write(out)
      remain  .write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      nStream .dispose()
      state   .dispose()
      remain  .dispose()
      valid   .dispose()
    }
  }
}