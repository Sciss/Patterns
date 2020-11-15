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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Grouped, Pat}
import de.sciss.serial.{DataInput, DataOutput}

object GroupedImpl extends StreamFactory {
  final val typeId = 0x47726F75 // "Grou"

  def expand[T <: Exec[T], A](pat: Grouped[A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val sizeStream  = size.expand[T]
    val innerStream = id.newVar[Pat[A]](null)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, sizeStream = sizeStream, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any](in)
    val sizeStream  = Stream.read[T, Int](in)
    val innerStream = id.readVar[Pat[Any]](in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, sizeStream = sizeStream, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id          : Ident[T],
                                                   inStream    : Stream[T, A],
                                                   sizeStream  : Stream[T, Int],
                                                   innerStream : Var[T, Pat[A]],
                                                   _hasNext    : Var[T, Boolean],
                                                   valid       : Var[T, Boolean]
  )
    extends Stream[T, Pat[A]] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream  )
      val sizeStreamOut   = c(sizeStream)
      val innerStreamOut  = idOut.newVar[Pat[A]](innerStream())
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())

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

    def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      inStream    .dispose()
      sizeStream  .dispose()
      innerStream .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      sizeStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      advance()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
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
        val inner     = Pat(b.result(): _*) // Stream[T, A](b.result: _*)
        innerStream() = inner
        _hasNext()    = sizeVal > 0
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}
