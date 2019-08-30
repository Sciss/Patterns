/*
 *  IndexItStream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package stream

import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput}

object IndexItStream extends StreamFactory {
  final val typeId = 0x49784974 // "IxIt"

  def expand[S <: Base[S]](token: Int)(implicit ctx: Context[S], tx: S#Tx): AdvanceItStream[S, Int] = {
    val id        = tx.newId()
    val iteration = tx.newIntVar(id, 0)
    val _hasNext  = tx.newBooleanVar(id, true)

    new Impl[S](id = id, token = token, iteration = iteration, _hasNext = _hasNext)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {

    val id        = tx.readId(in, access)
    val token     = in.readInt()
    val iteration = tx.readIntVar(id, in)
    val _hasNext  = tx.readBooleanVar(id, in)

    val res = new Impl[S](id = id, token = token, iteration = iteration, _hasNext = _hasNext)
    ctx.registerItStream(res)
    res
  }

  private final class Impl[S <: Base[S]](id: S#Id, val token: Int, iteration: S#Var[Int], _hasNext: S#Var[Boolean])
    extends AdvanceItStream[S, Int] {

    protected def typeId: Int = IndexItStream.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      out.writeInt(token)
      iteration .write(out)
      _hasNext  .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      iteration .dispose()
      _hasNext  .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit =
      _hasNext() = true

    def resetOuter()(implicit tx: S#Tx): Unit = {
      iteration() = 0
      _hasNext () = true
    }

    def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      iteration() = iteration() + 1
      _hasNext () = true
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

    def next()(implicit ctx: Context[S], tx: S#Tx): Int = {
      if (!hasNext) Stream.exhausted()
      _hasNext() = false
      iteration()
    }
  }
}