/*
 *  FlattenImpl.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Flatten
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object FlattenImpl extends StreamFactory {
  final val typeId = 0x466C6174 // "Flat"

  def expand[S <: Base[S], A](pat: Flatten[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in.expand[S]
    val hasInner    = tx.newBooleanVar(id, false)
    val innerStream = tx.newVar[Stream[S, A]](id, null)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, hasInner = hasInner, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Pat[Any]](in, access)
    val hasInner    = tx.readBooleanVar(id, in)
    val innerStream = tx.readVar[Stream[S, Any]](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, hasInner = hasInner, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id          : S#Id,
                                                   inStream    : Stream[S, Pat[A]],
                                                   hasInner    : S#Var[Boolean],
                                                   innerStream : S#Var[Stream[S, A]],
                                                   _hasNext    : S#Var[Boolean],
                                                   valid       : S#Var[Boolean]
  )
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream)
      val hasInnerOut     = txOut.newBooleanVar(idOut, hasInner())
      val innerStreamOut  = c.copyVar(idOut, innerStream)
      val hasNextOut      = txOut.newBooleanVar(idOut, _hasNext())
      val validOut        = txOut.newBooleanVar(idOut, valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, hasInner = hasInnerOut, innerStream = innerStreamOut,
        _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = FlattenImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      hasInner    .write(out)
      innerStream .write(out)
      _hasNext    .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      inStream    .dispose()
      hasInner    .dispose()
      innerStream .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      hasInner()  = false
      advance()
    }

    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      if (hasInner()) {
        hasInner() = innerStream().hasNext
      }

      _hasNext() = hasInner()
      if (!_hasNext()) {
        _hasNext() = inStream.hasNext
        if (_hasNext()) {
          val inPat     = inStream.next()
          innerStream() = inPat.expand
          hasInner() = true
          advance()
        }
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream().next()
      advance()
      res
    }
  }
}
