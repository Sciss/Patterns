/*
 *  SlidingImpl.scala
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
import de.sciss.patterns.graph.Sliding
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object SlidingImpl extends StreamFactory {
  final val typeId = 0x536C6964 // "Slid"

  def expand[S <: Base[S], A](pat: Sliding[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[S]
    val sizeStream  = size.expand[S]
    val stepStream  = step.expand[S]
    val innerStream = tx.newVar[Pat[A]](id, null)
    val hasStep     = tx.newBooleanVar(id, true)
    val buf         = tx.newVar[Vec[A]](id, Vector.empty)(PatElem.vecSerializer)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)
    
    new StreamImpl[S, A](id = id, inStream = inStream, sizeStream = sizeStream, stepStream = stepStream,
      innerStream = innerStream, hasStep = hasStep, buf = buf, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any  ](in, access)
    val sizeStream  = Stream.read[S, Int](in, access)
    val stepStream  = Stream.read[S, Int](in, access)
    val innerStream = tx.readVar[Pat[Any]](id, in)
    val hasStep     = tx.readBooleanVar(id, in)
    val buf         = tx.readVar[Vec[Any]](id, in)(PatElem.vecSerializer)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, sizeStream = sizeStream, stepStream = stepStream,
      innerStream = innerStream, hasStep = hasStep, buf = buf, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   val id          : S#Id,
                                                   val inStream    : Stream[S, A],
                                                   val sizeStream  : Stream[S, Int],
                                                   val stepStream  : Stream[S, Int],
                                                   val innerStream : S#Var[Pat[A]],
                                                   val hasStep     : S#Var[Boolean],
                                                   val buf         : S#Var[Vec[A]],
                                                   val _hasNext    : S#Var[Boolean],
                                                   val valid       : S#Var[Boolean]
  ) 
    extends Stream[S, Pat[A]] {

    protected def typeId: Int = SlidingImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      inStream    .write(out)
      sizeStream  .write(out)
      stepStream  .write(out)
      innerStream .write(out)
      hasStep     .write(out)
      buf         .write(out)
      _hasNext    .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      inStream    .dispose()
      sizeStream  .dispose()
      stepStream  .dispose()
      innerStream .dispose()
      hasStep     .dispose()
      buf         .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream  .reset()
      sizeStream.reset()
      stepStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      hasStep()  = true
      buf()      = Vector.empty
      advance()
    }

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val shn     = hasStep() && sizeStream.hasNext && inStream.hasNext
      _hasNext()  = shn
      if (shn) {
        val sizeVal = math.max(0, sizeStream.next())
        val b       = Vector.newBuilder[A]
        b.sizeHint(sizeVal)
        val vecOld  = buf()
        var i = 0
        while (i < sizeVal && i < vecOld.size) {
          b += vecOld(i)
          i += 1
        }
        while (i < sizeVal && inStream.hasNext) {
          b += inStream.next()
          i += 1
        }
        val vecNew    = b.result()
        val inner     = Pat(vecNew: _*) // Stream[S, A](vecNew: _*)
        innerStream() = inner
        val ihn       = sizeVal > 0
        _hasNext()    = ihn
        if (ihn) {
          val phn     = stepStream.hasNext
          hasStep()   = phn
          if (phn) {
            val stepVal = math.max(0, stepStream.next())
            buf()      = vecNew.drop(stepVal)
            while (i < stepVal && inStream.hasNext) {
              inStream.next()
              i += 1
            }
            hasStep()  = stepVal > 0
          }
        }
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
