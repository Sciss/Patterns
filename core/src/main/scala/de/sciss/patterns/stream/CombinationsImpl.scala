/*
 *  CombinationsImpl.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Combinations
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable

object CombinationsImpl extends StreamFactory {
  final val typeId = 0x436F6D62 // "Comb"

  def expand[S <: Base[S], A](pat: Combinations[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val nStream   = n .expand[S]
    val elements  = tx.newVar[Vec[A]]  (id, Vector.empty)(PatElem.vecSerializer)
    val counts    = tx.newVar[Vec[Int]](id, Vector.empty)
    val numbers   = tx.newVar[Vec[Int]](id, Vector.empty)
    val offsets   = tx.newVar[Vec[Int]](id, Vector.empty)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S, A](id = id, inStream = inStream, nStream = nStream, elements = elements, counts = counts,
      numbers = numbers, offsets = offsets, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any  ](in, access)
    val nStream   = Stream.read[S, Int](in, access)
    val elements  = tx.readVar[Vec[Any]]  (id, in)(PatElem.vecSerializer)
    val counts    = tx.readVar[Vec[Int]](id, in)
    val numbers   = tx.readVar[Vec[Int]](id, in)
    val offsets   = tx.readVar[Vec[Int]](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, inStream = inStream, nStream = nStream, elements = elements, counts = counts,
      numbers = numbers, offsets = offsets, _hasNext = _hasNext, valid = valid)
  }

  // Adapted from scala.collection.SeqLike#CombinationsItr
  // Scala license: BSD 3-clause
  //
  // generating all nums such that:
  // (1) nums(0) + .. + nums(length-1) = n
  // (2) 0 <= nums(i) <= cnts(i), where 0 <= i <= cnts.length-1
  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   nStream : Stream[S, Int],
                                                   elements: S#Var[Vec[A]],
                                                   counts  : S#Var[Vec[Int]],
                                                   numbers : S#Var[Vec[Int]],
                                                   offsets : S#Var[Vec[Int]],
                                                   _hasNext: S#Var[Boolean],
                                                   valid   : S#Var[Boolean]
  )
    extends Stream[S, Pat[A]] {

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, Pat[A]] = {
      val idOut       = txOut.newId()
      val inStreamOut = inStream.copyStream[Out]()
      val nStreamOut  = nStream .copyStream[Out]()
      val elementsOut = txOut.newVar[Vec[A]]  (idOut, elements())(PatElem.vecSerializer)
      val countsOut   = txOut.newVar[Vec[Int]](idOut, counts())
      val numbersOut  = txOut.newVar[Vec[Int]](idOut, numbers())
      val offsetsOut  = txOut.newVar[Vec[Int]](idOut, offsets())
      val hasNextOut  = txOut.newBooleanVar(idOut, _hasNext())
      val validOut    = txOut.newBooleanVar(idOut, valid())

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

    def dispose()(implicit tx: S#Tx): Unit = {
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

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
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
    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
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
