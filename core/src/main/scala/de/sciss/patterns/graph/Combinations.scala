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

import de.sciss.patterns.Types.Top

import scala.collection.mutable

final case class Combinations[T <: Top](in: Pat[T], n: Pat.Int) extends Pattern[Pat[T]] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, Pat[T] /* Stream[Tx, T#Out] */] = new Stream[Tx, Pat[T] /* Stream[Tx, T#Out] */] {
    // Adapted from scala.collection.SeqLike#CombinationsItr
    // Scala license: BSD 3-clause

    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private[this] val elements  = ctx.newVar[IndexedSeq[T#Out]](null)
    private[this] val counts    = ctx.newVar[Vector[Int]](null)
    private[this] val numbers   = ctx.newVar[Vector[Int]](null)
    private[this] val offsets   = ctx.newVar[Vector[Int]](null)
    private[this] val _hasNext  = ctx.newVar(false)

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Pat[T] /* Stream[Tx, T#Out] */ = {
      if (!_hasNext()) Stream.exhausted()

      /* Calculate this result. */
      val buf = List.newBuilder[T#Out]
      var _numbers  = numbers()
      val _elements = elements()
      val _offsets  = offsets()
      val _counts   = counts()

      for (k <- _numbers.indices; j <- 0 until _numbers(k)) {
        buf += _elements(_offsets(k) + j)
      }

      /* Prepare for the next call to next. */
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

      new Stream[Tx, T#Out] {
        private[this] val peer = buf.result().iterator

        def reset()(implicit tx: Tx): Unit    = ()
        def hasNext(implicit tx: Tx): Boolean = peer.hasNext
        def next ()(implicit tx: Tx): T#Out   = peer.next()
      }

      ???
    }

    private[this] val inStream: Stream[Tx, T#Out] = in.expand
    private[this] val nStream : Stream[Tx, Int]   = n .expand

    private[this] var nVal: Int = _

    private[this] val _valid = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    /* Rearranges seq to newSeq a0a0..a0a1..a1...ak..ak such that
     * seq.count(_ == aj) == counts(j)
     */
    private def validate()(implicit tx: Tx): Unit = if (!_valid()) {
      _valid() = true
      _hasNext() = nStream.hasNext
      if (!_hasNext()) return

      nVal = nStream.next()

      val m = mutable.Map.empty[T#Out, Int]

      // e => (e, weight(e))
      val tup: List[(T#Out, Int)] = inStream.map(e => (e, m.getOrElseUpdate(e, m.size))).toList.sortBy(_._2)
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
