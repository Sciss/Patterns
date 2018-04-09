/*
 *  GroupedImpl.scala
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
import de.sciss.patterns
import de.sciss.patterns.graph.Grouped
import de.sciss.serial.{DataInput, DataOutput}

object GroupedImpl extends StreamFactory {
  final val typeId = 0x47726F75 // "Grou"

  def expand[S <: Base[S], A](pat: Grouped[A])(implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, Pat[A]] = {
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

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val id          = tx.newId()
    val inStream    = patterns.Stream.read[S, A  ](in, access)
    val sizeStream  = patterns.Stream.read[S, Int](in, access)
    val innerStream = tx.readVar[Pat[A]](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, A](id = id, inStream = inStream, sizeStream = sizeStream, innerStream = innerStream,
      _hasNext = _hasNext, valid = valid)
      .asInstanceOf[patterns.Stream[S, A]] // XXX TODO --- ugly
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id          : S#Id,
                                                   inStream    : patterns.Stream[S, A],
                                                   sizeStream  : patterns.Stream[S, Int],
                                                   innerStream : S#Var[Pat[A]],
                                                   _hasNext    : S#Var[Boolean],
                                                   valid       : S#Var[Boolean]
  )
    extends Stream[S, Pat[A]] {

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
      if (!hasNext) patterns.Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}
