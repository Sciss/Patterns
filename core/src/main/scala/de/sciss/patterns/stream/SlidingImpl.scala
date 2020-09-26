/*
 *  SlidingImpl.scala
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
import de.sciss.patterns.graph.Sliding
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object SlidingImpl extends StreamFactory {
  final val typeId = 0x536C6964 // "Slid"

  def expand[T <: Exec[T], A](pat: Sliding[A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val id          = tx.newId()
    val inStream    = in  .expand[T]
    val sizeStream  = size.expand[T]
    val stepStream  = step.expand[T]
    val innerStream = id.newVar[Pat[A]](null)
    val hasStep     = id.newBooleanVar(true)
    val buf         = id.newVar[Vec[A]](Vector.empty)(tx, PatElem.vecFormat)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)
    
    new StreamImpl[T, A](id = id, inStream = inStream, sizeStream = sizeStream, stepStream = stepStream,
      innerStream = innerStream, hasStep = hasStep, buf = buf, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val inStream    = Stream.read[T, Any  ](in)
    val sizeStream  = Stream.read[T, Int](in)
    val stepStream  = Stream.read[T, Int](in)
    val innerStream = id.readVar[Pat[Any]](in)
    val hasStep     = id.readBooleanVar(in)
    val buf         = id.readVar[Vec[Any]](in)(PatElem.vecFormat)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, sizeStream = sizeStream, stepStream = stepStream,
      innerStream = innerStream, hasStep = hasStep, buf = buf, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   val id          : Ident[T],
                                                   val inStream    : Stream[T, A],
                                                   val sizeStream  : Stream[T, Int],
                                                   val stepStream  : Stream[T, Int],
                                                   val innerStream : Var[T, Pat[A]],
                                                   val hasStep     : Var[T, Boolean],
                                                   val buf         : Var[T, Vec[A]],
                                                   val _hasNext    : Var[T, Boolean],
                                                   val valid       : Var[T, Boolean]
  ) 
    extends Stream[T, Pat[A]] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val idOut           = txOut.newId()
      val inStreamOut     = c(inStream  )
      val sizeStreamOut   = c(sizeStream)
      val stepStreamOut   = c(stepStream)
      val innerStreamOut  = idOut.newVar[Pat[A]](innerStream())
      val hasStepOut      = idOut.newBooleanVar(hasStep())
      val bufOut          = idOut.newVar[Vec[A]](buf())(txOut, PatElem.vecFormat)
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, sizeStream = sizeStreamOut, stepStream = stepStreamOut,
        innerStream = innerStreamOut, hasStep = hasStepOut, buf = bufOut, _hasNext = hasNextOut, valid = validOut)
    }

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

    def dispose()(implicit tx: T): Unit = {
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

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream  .reset()
      sizeStream.reset()
      stepStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      hasStep()  = true
      buf()      = Vector.empty
      advance()
    }

    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
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
        val inner     = Pat(vecNew: _*) // Stream[T, A](vecNew: _*)
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
