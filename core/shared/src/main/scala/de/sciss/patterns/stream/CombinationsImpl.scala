/*
 *  CombinationsImpl.scala
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

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.{Combinations, Pat}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object CombinationsImpl extends StreamFactory {
  final val typeId = 0x436F6D62 // "Comb"

  def expand[T <: Exec[T], A](pat: Combinations[A])(implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val nStream   = n .expand[T]
    val elements  = id.newVar[Vec[A]]  (Vector.empty)(tx, PatElem.vecFormat)
    val counts    = id.newVar[Vec[Int]](Vector.empty)
    val numbers   = id.newVar[Vec[Int]](Vector.empty)
    val offsets   = id.newVar[Vec[Int]](Vector.empty)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, inStream = inStream, nStream = nStream, elements = elements, counts = counts,
      numbers = numbers, offsets = offsets, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any  ](in)
    val nStream   = Stream.read[T, Int](in)
    val elements  = id.readVar[Vec[Any]](in)(PatElem.vecFormat)
    val counts    = id.readVar[Vec[Int]](in)
    val numbers   = id.readVar[Vec[Int]](in)
    val offsets   = id.readVar[Vec[Int]](in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)

    new StreamImpl[T, Any](id = id, inStream = inStream, nStream = nStream, elements = elements, counts = counts,
      numbers = numbers, offsets = offsets, _hasNext = _hasNext, valid = valid)
  }

  // Adapted from scala.collection.SeqLike#CombinationsItr
  // Scala license: BSD 3-clause
  //
  // generating all nums such that:
  // (1) nums(0) + .. + nums(length-1) = n
  // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   nStream : Stream[T, Int],
                                                   elements: Var[T, Vec[A]],
                                                   counts  : Var[T, Vec[Int]],
                                                   numbers : Var[T, Vec[Int]],
                                                   offsets : Var[T, Vec[Int]],
                                                   _hasNext: Var[T, Boolean],
                                                   valid   : Var[T, Boolean]
  )
    extends Stream[T, Pat[A]] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Pat[A]] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val nStreamOut  = c(nStream )
      val elementsOut = idOut.newVar[Vec[A]]  (elements())(txOut, PatElem.vecFormat)
      val countsOut   = idOut.newVar[Vec[Int]](counts())
      val numbersOut  = idOut.newVar[Vec[Int]](numbers())
      val offsetsOut  = idOut.newVar[Vec[Int]](offsets())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, nStream = nStreamOut, elements = elementsOut, counts = countsOut,
        numbers = numbersOut, offsets = offsetsOut, _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = CombinationsImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      nStream .write(out)
      elements.write(out)
      counts  .write(out)
      numbers .write(out)
      offsets .write(out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      nStream .dispose()
      elements.dispose()
      counts  .dispose()
      numbers .dispose()
      offsets .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
      nStream .reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): Pat[A] = {
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
    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      _hasNext() = nStream.hasNext
      if (!_hasNext()) return

      val nVal = nStream.next()

      val m = mutable.Map.empty[A, Int]

      // e => (e, weight(e))
      val tup: List[(A, Int)] = inStream.toIterator.map(e => (e, m.getOrElseUpdate(e, m.size))).toList.sortBy(_._2)
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
