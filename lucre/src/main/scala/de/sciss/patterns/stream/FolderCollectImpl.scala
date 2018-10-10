/*
 *  FolderCollectImpl.scala
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
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.serial.{DataInput, DataOutput}

object FolderCollectImpl extends StreamFactory {
  final val typeId = 0x466C436C // "FlCl"

  def expand[S <: Base[S], A](pat: graph.Folder.Collect[A])
                             (implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    val id        = tx.newId()
//    val keyStream = folder.key.expand[S]
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
//    ctx.requestInput(Context.Input.Attribute(folder.key))

//    new StreamImpl[S, A](id = id, keyStream = keyStream, _hasNext = _hasNext, valid = valid)
    ???
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val keyStream = Stream.read[S, String](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, keyStream = keyStream, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id       : S#Id,
                                                   keyStream: Stream[S, String],
                                                   _hasNext : S#Var[Boolean],
                                                   valid    : S#Var[Boolean]
                                                 )
    extends Stream[S, A] {

    protected def typeId: Int = AudioCueNumFramesImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      keyStream.write(out)
      _hasNext.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      keyStream.dispose()
      _hasNext.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      keyStream.reset()
      _hasNext() = true
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      advance()
    }

//    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val khn = keyStream.hasNext
      if (khn) {
        val keyVal = keyStream.next()
        val vf = ctx.requestInput(LContext.Attribute[graph.Folder](keyVal))
        vf.peer match {
          case Some(_) =>
          case _ =>
        }
//        val ihn = inStream.hasNext
//        if (ihn) {
//          val inVal = inStream.next()
//          state()   = inVal
//          remain()  = keyVal
//        } else {
//          remain() = 0
//        }
//
      } else {
        ???
      }
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      var res = 0
      while (keyStream.hasNext) {
        keyStream.next()
        res += 1
      }
      _hasNext() = false
      res
      ???
    }
  }
}
