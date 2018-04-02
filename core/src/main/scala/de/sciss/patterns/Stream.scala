/*
 *  Stream.scala
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

import de.sciss.patterns.Context.Var

import scala.annotation.tailrec

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  def fill[Tx, A](n: Int)(elem: => A)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    continually(elem).take(n) // XXX TODO -- more efficient

  def empty[Tx, A]: Stream[Tx, A] = new Stream[Tx, A] {
    override def toString = "Stream.empty"

    def reset()(implicit tx: Tx): Unit    = ()
    def hasNext(implicit tx: Tx): Boolean = false
    def next ()(implicit tx: Tx): A       = Stream.exhausted()
  }

//  def reverseIterator[Tx, A](that: Iterable[A]): Stream[Tx, A] = ...

  def continually[Tx, A](elem: => A): Stream[Tx, A] = new Stream[Tx, A] {
    override def toString = s"Stream.continually@${hashCode().toHexString}"

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = true
    def next ()(implicit tx: Tx): A       = elem
  }

  def single[Tx, A](elem: A)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new Single[Tx, A](elem, tx)

  private final class Single[Tx, A](elem: A, tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val _hasNext = ctx.newBooleanVar(true)(tx0)

    private def simpleString = s"Stream.single($elem)"

    override def toString = s"$simpleString; hasNext = ${_hasNext}"

    def hasNext(implicit tx: Tx): Boolean = _hasNext()

    def reset()(implicit tx: Tx): Unit =
      _hasNext() = true

    def next()(implicit tx: Tx): A = {
      if (!_hasNext()) Stream.exhausted()
      _hasNext() = false
      logStream(s"$simpleString.next()")
      elem
    }
  }

  def apply[Tx, A](elems: A*)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new Seq(elems, tx)

  private final class Seq[Tx, A](elems: scala.Seq[A], tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val count = ctx.newIntVar(0)(tx0)
    private[this] val xs    = elems.toIndexedSeq

    private[this] lazy val simpleString =
      elems.mkString("Stream(", ", ", ")")

    override def toString = s"$simpleString; count = $count"

    def reset()(implicit tx: Tx): Unit =
      count() = 0

    def hasNext(implicit tx: Tx): Boolean = count() < xs.size

    def next ()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val i = count()
      count() = i + 1
      val res = elems(i)
      // logStream(s"$simpleString.next(); count = $i; res = $res")
      res
    }
  }

  private final class FlatMap[Tx, A, B](outer: Stream[Tx, A], f: A => Stream[Tx, B], tx0: Tx)
                                       (implicit ctx: Context[Tx])
    extends Stream[Tx, B] {

    private[this] val _valid    = ctx.newBooleanVar(false)(tx0)
    private[this] val _hasNext  = ctx.newBooleanVar(false)(tx0)
    private[this] val sub       = ??? : Var[Tx, Stream[Tx, B]] // ctx.newVar[Stream[Tx, B]](null)(tx0)
    //    private[this] val _hasSub   = ctx.newVar(false)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      outer.reset()
      //        if (_hasSub()) sub().reset()
    }

    @tailrec
    private def step()(implicit tx: Tx): Unit = {
      val ohn = outer.hasNext
      if (ohn) {
        val _sub    = f(outer.next())
        sub()       = _sub
        //        _hasSub()   = true
        val shn     = _sub.hasNext
        _hasNext()  = shn
        if (!shn) step()
      } else {
        //        _hasSub()   = false
        _hasNext()  = false
      }
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid() = true
        step()
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): B = {
      if (!hasNext) Stream.exhausted()
      val _sub = sub()
      val res = _sub.next()
      if (!_sub.hasNext) step()
      res
    }
  }

  private final class Map[Tx, A, B](outer: Stream[Tx, A], f: A => B) extends Stream[Tx, B] {
    def reset()(implicit tx: Tx): Unit    = outer.reset()
    def hasNext(implicit tx: Tx): Boolean = outer.hasNext
    def next ()(implicit tx: Tx): B       = f(outer.next())
  }

  private final class Zip[Tx, A, B](outer: Stream[Tx, A], that: Stream[Tx, B]) extends Stream[Tx, (A, B)] {
    def reset()(implicit tx: Tx): Unit = {
      outer.reset()
      that .reset()
    }

    def hasNext(implicit tx: Tx): Boolean = outer.hasNext && that.hasNext

    def next()(implicit tx: Tx): (A, B) = (outer.next(), that.next())
  }

  private final class ++ [Tx, A, B >: A](outer: Stream[Tx, A], that: Stream[Tx, B]) extends Stream[Tx, B] {
    def reset()(implicit tx: Tx): Unit = {
      outer.reset()
      that .reset()
    }

    def hasNext(implicit tx: Tx): Boolean = outer.hasNext || that.hasNext

    def next()(implicit tx: Tx): B =
      if (outer.hasNext) outer.next() else that.next()
  }

  private final class Take[Tx, A](outer: Stream[Tx, A], n: Int, tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A] {

    private[this] val i = ctx.newIntVar(0)(tx0)

    def reset()(implicit tx: Tx): Unit = outer.reset()

    def hasNext(implicit tx: Tx): Boolean = i() < n && outer.hasNext

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      i() = i() + 1
      outer.next()
    }
  }
}
abstract class Stream[Tx, +A] { outer =>
//  Stream.COUNT += 1

  def reset()(implicit tx: Tx): Unit

  def hasNext(implicit tx: Tx): Boolean
  def next ()(implicit tx: Tx): A

  def map[B](f: A => B)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, B] = new Stream.Map(outer, f)

  def flatMap[B](f: A => Stream[Tx, B])(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, B] =
    new Stream.FlatMap(outer, f, tx)

  def zip[B](that: Stream[Tx, B])(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, (A, B)] =
    new Stream.Zip(outer, that)

  def ++ [B >: A](that: Stream[Tx, B]): Stream[Tx, B] = new Stream.++(outer, that)

  def take(n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new Stream.Take(outer, n, tx)

  def drop(n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    var j = 0
    while (j < n && hasNext) {
      next()
      j += 1
    }
    this
  }

  def isEmpty (implicit tx: Tx): Boolean = !hasNext
  def nonEmpty(implicit tx: Tx): Boolean = hasNext

  def toList(implicit tx: Tx): List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  def toVector(implicit tx: Tx): Vector[A] = {
    val b = Vector.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  def size(implicit tx: Tx): Int = {
    var res = 0
    while (hasNext) {
      next()
      res += 1
    }
    res
  }

  def foreach(fun: A => Unit)(implicit tx: Tx): Unit =
    while (hasNext) fun(next())
}
