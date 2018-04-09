/*
 *  BrownImpl.scala
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

import de.sciss.lucre.stm.{Base, Random, TxnRandom}
import de.sciss.patterns
import de.sciss.patterns.Types.{Aux, Num, Widen2}
import de.sciss.patterns.graph.Brown
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object BrownImpl extends StreamFactory {
  final val typeId = 0x42726F77 // "Brow"

  def expand[S <: Base[S], A1, A2, A](pat: Brown[A1, A2, A])(implicit ctx: Context[S], tx: S#Tx): patterns.Stream[S, A] = {
    import pat._
    val id          = tx.newId()

    val loStream    = lo.expand[S]
    val hiStream    = hi.expand[S]
    val stepStream  = step.expand[S]
    val state       = PatElem.makeVar[S, A](id)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newBooleanVar(id, false)
    val r           = ctx.mkRandom(ref)

    new StreamImpl[S, A1, A2, A](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): patterns.Stream[S, A] = {
    val id          = tx.readId(in, access)
    val loStream    = patterns.Stream.read[S, Any](in, access)
    val hiStream    = patterns.Stream.read[S, Any](in, access)
    val stepStream  = patterns.Stream.read[S, A](in, access)
    val state       = PatElem.readVar[S, A](id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readBooleanVar(id, in)
    val r           = TxnRandom.read[S](in, access)
    val widen       = Aux.readT[Widen2[Any, Any, A]](in)
    val num         = Aux.readT[Num[A]](in)

    new StreamImpl[S, Any, Any, A](id = id, loStream = loStream, hiStream = hiStream, stepStream = stepStream,
      state = state, _hasNext = _hasNext,
      valid = valid)(r, widen, num)
  }

  private final class StreamImpl[S <: Base[S], A1, A2, A](
                                                           id        : S#Id,
                                                           loStream  : patterns.Stream[S, A1],
                                                           hiStream  : patterns.Stream[S, A1],
                                                           stepStream: patterns.Stream[S, A2],
                                                           state     : S#Var[A],
                                                           _hasNext  : S#Var[Boolean],
                                                           valid     : S#Var[Boolean]
  )(
    implicit r: TxnRandom[S],
    widen: Widen2[A1, A2, A],
    num: Num[A]
  )
    extends Stream[S, A] {

    import widen._

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

    def dispose()(implicit tx: S#Tx): Unit = {
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
    private def calcNext(cur: A, step: A)(implicit r: Random[S#Tx], tx: S#Tx): A =
      num.+(cur, num.rand2(step))

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      _hasNext() = loStream.hasNext && hiStream.hasNext
      if (_hasNext()) {
        state() = num.rrand(widen1(loStream.next()), widen1(hiStream.next()))
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      loStream  .reset()
      hiStream  .reset()
      stepStream.reset()
      // XXX TODO: r.reset()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) patterns.Stream.exhausted()
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
