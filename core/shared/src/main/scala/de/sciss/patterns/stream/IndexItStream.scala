/*
 *  IndexItStream.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object IndexItStream extends StreamFactory {
  final val typeId = 0x49784974 // "IxIt"

  def expand[T <: Exec[T]](token: Int)(implicit ctx: Context[T], tx: T): AdvanceItStream[T, Int] = {
    val id        = tx.newId()
    val iteration = id.newIntVar(0)
    val _hasNext  = id.newBooleanVar(true)
    new Impl[T](id = id, token = token, iteration = iteration, _hasNext = _hasNext)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {

    val id        = tx.readId(in)
    val token     = in.readInt()
    val iteration = id.readIntVar(in)
    val _hasNext  = id.readBooleanVar(in)

    val res = new Impl[T](id = id, token = token, iteration = iteration, _hasNext = _hasNext)
    ctx.registerItStream(res)
    res
  }

  private final class Impl[T <: Exec[T]](id: Ident[T], val token: Int, iteration: Var[T, Int], _hasNext: Var[T, Boolean])
    extends AdvanceItStream[T, Int] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Int] = {
      val idOut         = txOut.newId()
      val iterationOut  = idOut.newIntVar     (iteration())
      val hasNextOut    = idOut.newBooleanVar (_hasNext())
      new Impl[Out](id = idOut, token = token, iteration = iterationOut, _hasNext = hasNextOut)
    }


    protected def typeId: Int = IndexItStream.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      out.writeInt(token)
      iteration .write(out)
      _hasNext  .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      iteration .dispose()
      _hasNext  .dispose()
    }

    def reset()(implicit tx: T): Unit =
      _hasNext() = true

    def resetOuter()(implicit tx: T): Unit = {
      iteration() = 0
      _hasNext () = true
    }

    def advance()(implicit ctx: Context[T], tx: T): Unit = {
      iteration() = iteration() + 1
      _hasNext () = true
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = _hasNext()

    def next()(implicit ctx: Context[T], tx: T): Int = {
      if (!hasNext) Stream.exhausted()
      _hasNext() = false
      iteration()
    }
  }
}