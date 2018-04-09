/*
 *  FlatTabulate.scala
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
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.IndexItStream
import de.sciss.serial.DataOutput

import scala.annotation.tailrec

final case class LoopWithIndex[A] private[patterns](n: Pat[Int], it: It[Int], inner: Pat[A] /* , innerLevel: Int */)
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val nT      = t(n)
    val innerT  = t(inner)
    if (nT.eq(n) && innerT.eq(inner)) this else copy(n = nT, inner = innerT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    @transient final private[this] lazy val ref = new AnyRef

    private[this] val id          = tx0.newId()
    private[this] val iteration   = tx0.newIntVar(id, 0)
    private[this] val nValue      = tx0.newIntVar(id, 0)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _valid      = tx0.newBooleanVar(id, false)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private def mkItStream(implicit tx: S#Tx) = {
      ???
//      val res = new IndexItStream[S](iteration, tx)
//      ctx.addStream(ref, res)
//      res
    }

    ???
//    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val nStream     = n    .expand(ctx, tx0)
    private[this] val innerStream = inner.expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      nStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        val nhn       = nStream.hasNext
        _hasNext()    = nhn
        if (nhn) {
          nValue()    = nStream.next()
          iteration() = 0
          nextIteration()
        }
      }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    @tailrec
    private def nextIteration()(implicit tx: S#Tx): Unit = {
      val i       = iteration()
      val n       = nValue()
      val nhn     = i < n
      _hasNext()  = nhn
      if (nhn) {
        ???
//        val itStreams = ctx.getStreams(ref)
//        itStreams.foreach(_.reset())
        innerStream.reset()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        if (!ihn) {
          iteration() = i + 1
          nextIteration()
        }
      }
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream.next()
      val ihn = innerStream.hasNext
      _hasNext() = ihn
      if (!ihn && iteration() < nValue()) {
        iteration() = iteration() + 1
        nextIteration()
      }
      res
    }
  }
}
