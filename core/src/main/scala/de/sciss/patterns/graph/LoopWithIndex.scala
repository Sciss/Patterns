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

import de.sciss.patterns.graph.impl.IndexItStream

import scala.annotation.tailrec

final case class LoopWithIndex[A] private[patterns](n: Pat[Int], it: It[Int], inner: Pat[A] /* , innerLevel: Int */)
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val nT      = t(n)
    val innerT  = t(inner)
    if (nT.eq(n) && innerT.eq(inner)) this else copy(n = nT, inner = innerT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    @transient final private[this] lazy val ref = new AnyRef

    private[this] val iteration = ctx.newVar(0)(tx0)

    private def mkItStream(implicit tx: Tx) = {
      val res = new IndexItStream(iteration, tx)
      ctx.addStream(ref, res)
      res
    }

    ctx.provideOuterStream(it.token, mkItStream(_))(tx0)

    private[this] val nStream     = n    .expand(ctx, tx0)
    private[this] val innerStream = inner.expand(ctx, tx0)

    private[this] val nValue      = ctx.newVar(0)(tx0)
    private[this] val _hasNext    = ctx.newVar(false)(tx0)
    private[this] val _valid      = ctx.newVar(false)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      nStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
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

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    @tailrec
    private def nextIteration()(implicit tx: Tx): Unit = {
      val i       = iteration()
      val n       = nValue()
      val nhn     = i < n
      _hasNext()  = nhn
      if (nhn) {
//        if (n == 200000 || n == 2000000) {
//          println("here")
//        }
//        val itStream = () => Stream.single(i)
//        ctx.provideOuterStream(it.token, itStream)
        val itStreams = ctx.getStreams(ref)
        itStreams.foreach(_.reset())
        innerStream.reset()
        val ihn = innerStream.hasNext
        _hasNext() = ihn
        if (!ihn) {
          iteration() = i + 1
          nextIteration()
        }
      }
    }

    def next()(implicit tx: Tx): A = {
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
