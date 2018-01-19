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

  def iterator(implicit ctx: Context): Stream[Stream[T#Out]] = new Stream[Stream[T#Out]] {
    // Adapted from scala.collection.SeqLike#CombinationsItr
    // Scala license: BSD 3-clause

    // generating all nums such that:
    // (1) nums(0) + .. + nums(length-1) = n
    // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
    private[this] var elements: IndexedSeq[T#Out] = _
    private[this] var counts  : Array[Int]        = _
    private[this] var numbers : Array[Int]        = _
    private[this] var offsets : Array[Int]        = _
    private[this] var _hasNext: Boolean           = _

    def hasNext: Boolean = _hasNext

    def next(): Stream[T#Out] = {
      if (!_hasNext) Stream.exhausted()

      /* Calculate this result. */
      val buf = List.newBuilder[T#Out]
      for (k <- numbers.indices; j <- 0 until numbers(k)) {
        buf += elements(offsets(k) + j)
      }

      /* Prepare for the next call to next. */
      var idx: Int = numbers.length - 1
      while (idx >= 0 && numbers(idx) == counts(idx))
        idx -= 1

      idx = numbers.lastIndexWhere(_ > 0, idx - 1)

      if (idx < 0)
        _hasNext = false
      else {
        // OPT: hand rolled version of `sum = nums.view(idx + 1, nums.length).sum + 1`
        var sum = 1
        var i = idx + 1
        while (i < numbers.length) {
          sum += numbers(i)
          i += 1
        }
        numbers(idx) -= 1
        for (k <- (idx + 1) until numbers.length) {
          numbers(k) = math.min(sum, counts(k))
          sum -= numbers(k)
        }
      }

      new Stream[T#Out] {
        private[this] val peer = buf.result().iterator

        def reset(): Unit = ()

        def hasNext: Boolean = peer.hasNext

        def next(): T#Out = peer.next()
      }
    }

    private[this] val inStream: Stream[T#Out] = in.expand
    private[this] val nStream : Stream[Int]   = n .expand

    private[this] var nVal: Int = _

    /** Rearranges seq to newSeq a0a0..a0a1..a1...ak..ak such that
      * seq.count(_ == aj) == counts(j)
      */
    def reset(): Unit = {
      _hasNext = nStream.hasNext
      if (!_hasNext) return

      nVal = nStream.next()

      val m = mutable.Map.empty[T#Out, Int]

      // e => (e, weight(e))
      val tup = inStream.map(e => (e, m.getOrElseUpdate(e, m.size))).toList.sortBy(_._2)
      val (es, is) = tup.unzip
      val cs = new Array[Int](m.size)
      is.foreach(i => cs(i) += 1)
      val ns = new Array[Int](cs.length)

      var r = nVal
      ns.indices.foreach { k =>
        ns(k) = math.min(r, cs(k))
        r -= ns(k)
      }

      elements  = es.toIndexedSeq
      counts    = cs
      numbers   = ns
      offsets   = counts.scanLeft(0)(_ + _)
    }

    reset()
  }
}
