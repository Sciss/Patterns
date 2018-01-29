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

import scala.annotation.tailrec

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  def fill[Tx, A](n: Int)(elem: => A)(implicit ctx: Context[Tx]): Stream[Tx, A] =
    continually(elem).take(n) // XXX TODO -- more efficient

  def empty[Tx, A]: Stream[Tx, A] = new Stream[Tx, A] {
    def reset()(implicit tx: Tx): Unit    = ()
    def hasNext(implicit tx: Tx): Boolean = false
    def next ()(implicit tx: Tx): A       = Stream.exhausted()
  }

//  def reverseIterator[Tx, A](that: Iterable[A]): Stream[Tx, A] = ...

  def continually[Tx, A](elem: => A): Stream[Tx, A] = new Stream[Tx, A] {
    def reset()(implicit tx: Tx): Unit    = ()
    def hasNext(implicit tx: Tx): Boolean = true
    def next ()(implicit tx: Tx): A       = elem
  }

  def single[Tx, A](elem: A)(implicit ctx: Context[Tx]): Stream[Tx, A] = new Stream[Tx, A] {
    private[this] val _hasNext = ctx.newVar(true)

    private def simpleString = s"Stream.single($elem)"

    override def toString = s"$simpleString; hasNext = ${_hasNext}"

    def hasNext(implicit tx: Tx): Boolean = _hasNext()

    def reset()(implicit tx: Tx): Unit = _hasNext() = true

    def next()(implicit tx: Tx): A = {
      if (!_hasNext()) Stream.exhausted()
      _hasNext() = false
      logStream(s"$simpleString.next()")
      elem
    }
  }

  def apply[Tx, A](elems: A*)(implicit ctx: Context[Tx]): Stream[Tx, A] = new Stream[Tx, A] {
    private[this] val count = ctx.newVar(0)
    private[this] val xs    = elems.toIndexedSeq

    // override def toString = s"$simpleString; count = $count"

    def reset()(implicit tx: Tx): Unit    = count() = 0
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
}
abstract class Stream[Tx, +A] { outer =>
  def reset()(implicit tx: Tx): Unit
  def hasNext(implicit tx: Tx): Boolean
  def next ()(implicit tx: Tx): A

  def map[B](f: A => B): Stream[Tx, B] = new Stream[Tx, B] {
    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = outer.hasNext

    def next()(implicit tx: Tx): B = f(outer.next())
  }

  def flatMap[B](f: A => Stream[Tx, B])(implicit ctx: Context[Tx]): Stream[Tx, B] = new Stream[Tx, B] {

    private[this] val _valid    = ctx.newVar(false)
    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val sub       = ctx.newVar[Stream[Tx, B]](null)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    @tailrec
    private def step()(implicit tx: Tx): Unit = {
      _hasNext() = outer.hasNext
      if (_hasNext()) {
        sub() = f(outer.next())
        _hasNext() = sub().hasNext
        if (!_hasNext()) step()
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
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = sub().next()
      if (!sub().hasNext) step()
      res
    }
  }

  def zip[B](that: Stream[Tx, B]): Stream[Tx, (A, B)] = new Stream[Tx, (A, B)] {
    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = outer.hasNext && that.hasNext

    def next()(implicit tx: Tx): (A, B) = (outer.next(), that.next())
  }

  def ++ [B >: A](that: Stream[Tx, B]): Stream[Tx, B] = new Stream[Tx, B] {
    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = outer.hasNext || that.hasNext

    def next()(implicit tx: Tx): B =
      if (outer.hasNext) outer.next() else that.next()
  }

  def take(n: Int)(implicit ctx: Context[Tx]): Stream[Tx, A] = new Stream[Tx, A] {
    private[this] val i = ctx.newVar(0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = i() < n && outer.hasNext

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      i() = i() + 1
      outer.next()
    }
  }

  def drop(n: Int)(implicit tx: Tx): Stream[Tx, A] = {
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
