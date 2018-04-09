/*
 *  StutterImpl.scala
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
import de.sciss.patterns.graph.Stutter
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object StutterImpl extends StreamFactory {
  final val typeId = 0x53747574 // "Stut"

  def expand[S <: Base[S], A](pat: Stutter[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val nStream   = n .expand[S]
    val state     = PatElem.makeVar[S, A](id)
    val remain    = tx.newIntVar    (id, 0)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, nStream = nStream, state = state, remain = remain,
      valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any  ](in, access)
    val nStream   = Stream.read[S, Int](in, access)
    val state     = PatElem.readVar[S, Any](id, in)
    val remain    = tx.readIntVar    (id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, nStream = nStream, state = state, remain = remain,
      valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   protected val id        : S#Id,
                                                   protected val inStream  : Stream[S, A],
                                                   protected val nStream   : Stream[S, Int],
                                                   protected val state     : S#Var[A],
                                                   protected val remain    : S#Var[Int],
                                                   protected val valid     : S#Var[Boolean]
                                                 )
    extends Stream[S, A] {

    protected def typeId: Int = StutterImpl.typeId

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
      nStream .reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      remain() > 0
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
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

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      nStream .dispose()
      state   .dispose()
      remain  .dispose()
      valid   .dispose()
    }
  }
}