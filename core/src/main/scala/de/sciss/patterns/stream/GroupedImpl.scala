/*
 *  GroupedImpl.scala
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
import de.sciss.patterns.graph.Grouped
import de.sciss.serial.{DataInput, DataOutput}

object GroupedImpl extends StreamFactory {
  final val typeId = 0x47726F75 // "Grou"

  def expand[S <: Base[S], A](pat: Grouped[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val sizeStream  = size.expand[S]
    val innerStream = tx.newVar[Pat[A]](id, null)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, sizeStream = sizeStream, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any](in, access)
    val sizeStream  = Stream.read[S, Int](in, access)
    val innerStream = tx.readVar[Pat[Any]](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, sizeStream = sizeStream, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id          : S#Id,
                                                   inStream    : Stream[S, A],
                                                   sizeStream  : Stream[S, Int],
                                                   innerStream : S#Var[Pat[A]],
                                                   _hasNext    : S#Var[Boolean],
                                                   valid       : S#Var[Boolean]
  )
    extends Stream[S, Pat[A]] {

    private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                      (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream  )
      val sizeStreamOut   = c(sizeStream)
      val innerStreamOut  = txOut.newVar[Pat[A]](idOut, innerStream())
      val hasNextOut      = txOut.newBooleanVar(idOut, _hasNext())
      val validOut        = txOut.newBooleanVar(idOut, valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, sizeStream = sizeStreamOut,
        innerStream = innerStreamOut, _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = GroupedImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      sizeStream  .write(out)
      innerStream .write(out)
      _hasNext    .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      inStream    .dispose()
      sizeStream  .dispose()
      innerStream .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      sizeStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      advance()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      _hasNext() = sizeStream.hasNext && inStream.hasNext
      if (_hasNext()) {
        val sizeVal = math.max(0, sizeStream.next())
        val b       = Vector.newBuilder[A]
        b.sizeHint(sizeVal)
        var i = 0
        // there is _no_ reasonable way to provide the
        // stream than to eagerly collect the values here,
        // because of the order of execution between inner and outer
        // `next`!
        while (i < sizeVal && inStream.hasNext) {
          b += inStream.next()
          i += 1
        }
        val inner     = Pat(b.result: _*) // Stream[S, A](b.result: _*)
        innerStream() = inner
        _hasNext()    = sizeVal > 0
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}
