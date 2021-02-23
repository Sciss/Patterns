/*
 *  BrownImpl.scala
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

import de.sciss.lucre.{Adjunct, Exec, Ident, Random, RandomObj, Var}
import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.patterns.graph.Brown
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object BrownImpl extends StreamFactory {
  final val typeId = 0x42726F77 // "Brow"

  def expand[T <: Exec[T], A1, A2, A](pat: Brown[A1, A2, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val loStream    = lo.expand[T]
    val hiStream    = hi.expand[T]
    val stepStream  = step.expand[T]
    val state       = PatElem.makeVar[T, A](id)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newBooleanVar(false)
    val r           = ctx.mkRandom(ref)

    new StreamImpl[T, A1, A2, A](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val loStream    = Stream.read[T, Any](in)
    val hiStream    = Stream.read[T, Any](in)
    val stepStream  = Stream.read[T, Any](in)
    val state       = PatElem.readVar[T, Any](id, in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readBooleanVar(in)
    val r           = RandomObj.read[T](in)
    val widen       = Adjunct.readT[Widen2[Any, Any, Any]](in)
    val num         = Adjunct.readT[Num[Any]](in)

    new StreamImpl[T, Any, Any, Any](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  private final class StreamImpl[T <: Exec[T], A1, A2, A](
                                                           id        : Ident[T],
                                                           loStream  : Stream[T, A1],
                                                           hiStream  : Stream[T, A1],
                                                           stepStream: Stream[T, A2],
                                                           state     : Var[T, A],
                                                           _hasNext  : Var[T, Boolean],
                                                           valid     : Var[T, Boolean]
  )(
    implicit r: RandomObj[T],
    widen: Widen2[A1, A2, A],
    num: Num[A]
  )
    extends Stream[T, A] {

    import widen._

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut         = txOut.newId()
      val loStreamOut   = c(loStream)
      val hiStreamOut   = c(hiStream)
      val stepStreamOut = c(stepStream)
      val stateOut      = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut    = idOut.newBooleanVar(_hasNext())
      val validOut      = idOut.newBooleanVar(valid())
      val rOut          = r.copy[Out]()

      new StreamImpl[Out, A1, A2, A](id = idOut, loStream = loStreamOut, hiStream = hiStreamOut, stepStream = stepStreamOut,
        state = stateOut, _hasNext = hasNextOut,
        valid = validOut)(rOut, widen, num)
    }

    protected def typeId: Int = BrownImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      loStream  .write(out)
      hiStream  .write(out)
      stepStream.write(out)
      state     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      r         .write(out)
      widen     .write(out)
      num       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      loStream  .dispose()
      hiStream  .dispose()
      stepStream.dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
      r         .dispose()
    }

    @inline
    private def calcNext(cur: A, step: A)(implicit r: Random[T], tx: T): A =
      num.plus(cur, num.rand2(step))

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = num.rangeRand(widen1(loStream.next()), widen1(hiStream.next()))
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      loStream  .reset()
      hiStream  .reset()
      stepStream.reset()
      // XXX TODO: r.reset()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      _hasNext() = loStream.hasNext && hiStream.hasNext && stepStream.hasNext
      if (_hasNext()) {
        val loVal   = loStream.next()
        val hiVal   = hiStream.next()
        val stepVal = stepStream.next()
        val x       = calcNext(res, widen2(stepVal))
        state()     = num.fold(x, widen1(loVal), widen1(hiVal))
      }
      res
    }
  }
}
