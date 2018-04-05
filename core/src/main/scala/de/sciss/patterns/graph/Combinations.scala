/*
 *  Combinations.scala
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
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

final case class Combinations[A](in: Pat[A], n: Pat[Int]) extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT = t(in)
    val nT  = t(n)
    if (inT.eq(in) && nT.eq(n)) this else copy(in = inT, n = nT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, Pat[A]] {

    private[this] val id        = tx0.newId()

    // Adapted from scala.collection.SeqLike#CombinationsItr
    // Scala license: BSD 3-clause

    private[this] val inStream: Stream[S, A]   = in.expand(ctx, tx0)
    private[this] val nStream : Stream[S, Int] = n .expand(ctx, tx0)

    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private[this] val elements  = tx0.newVar[Vec[A]]  (id, Vector.empty)(PatElem.vecSerializer)
    private[this] val counts    = tx0.newVar[Vec[Int]](id, Vector.empty)(Serializer.immutable) // Serial issue #1
    private[this] val numbers   = tx0.newVar[Vec[Int]](id, Vector.empty)(Serializer.immutable) // Serial issue #1
    private[this] val offsets   = tx0.newVar[Vec[Int]](id, Vector.empty)(Serializer.immutable) // Serial issue #1
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val _valid    = tx0.newBooleanVar(id, false)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
      nStream .reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()

      /* Calculate this result. */
      val buf = List.newBuilder[A]
      var _numbers  = numbers()
      val _elements = elements()
      val _offsets  = offsets()
      val _counts   = counts()

      for (k <- _numbers.indices; j <- 0 until _numbers(k)) {
        buf += _elements(_offsets(k) + j)
      }

      /* Prepare for the next call to `next`. */
      var idx: Int = _numbers.length - 1
      while (idx >= 0 && _numbers(idx) == _counts(idx))
        idx -= 1

      idx = _numbers.lastIndexWhere(_ > 0, idx - 1)

      if (idx < 0) {
        _hasNext() = false
      } else {
        // OPT: hand rolled version of `sum = nums.view(idx + 1, nums.length).sum + 1`
        var sum = 1
        var i = idx + 1
        while (i < _numbers.length) {
          sum += _numbers(i)
          i += 1
        }
        _numbers = _numbers.updated(idx, _numbers(idx) - 1)
        for (k <- (idx + 1) until _numbers.length) {
//          _numbers(k) = math.min(sum, _counts(k))
          val n = math.min(sum, _counts(k))
          _numbers = _numbers.updated(k, n)
          sum -= n
        }
        numbers() = _numbers
      }

      Pat(buf.result(): _*)
    }

    /* Rearranges seq to newSeq a0a0..a0a1..a1...ak..ak such that
     * seq.count(_ == aj) == counts(j)
     */
    private def validate()(implicit tx: S#Tx): Unit = if (!_valid()) {
      _valid() = true
      _hasNext() = nStream.hasNext
      if (!_hasNext()) return

      val nVal = nStream.next()

      val m = mutable.Map.empty[A, Int]

      // e => (e, weight(e))
      val tup: List[(A, Int)] = inStream.map(e => (e, m.getOrElseUpdate(e, m.size))).toList.sortBy(_._2)
      val (es, is) = tup.unzip
      var cs = Vector.fill(m.size)(0) //  new Array[Int](m.size)
      is.foreach { i =>
//        cs(i) += 1
        cs = cs.updated(i, cs(i) + 1)
      }

      var r = nVal
      val ns = Vector.tabulate(cs.length) { k =>
        val res = math.min(r, cs(k))
        r -= res
        res
      }

      elements()  = es.toIndexedSeq
      counts  ()  = cs
      numbers ()  = ns
      offsets ()  = cs.scanLeft(0)(_ + _)
    }
  }
}
