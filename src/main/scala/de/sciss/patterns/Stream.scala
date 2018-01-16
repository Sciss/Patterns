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

  def fill[A](n: Int)(elem: => A): Stream[A] =
    continually(elem).take(n) // XXX TODO -- more efficient

  def empty[A]: Stream[A] = new Stream[A] {
    def reset(): Unit = ()

    def hasNext: Boolean = false

    def next(): A = Stream.exhausted()
  }

  def reverseIterator[A](that: Iterable[A]): Stream[A] = ???

  def continually[A](elem: => A): Stream[A] = new Stream[A] {
    def reset(): Unit = ()

    def hasNext: Boolean = true

    def next(): A = elem
  }

  def single[A](elem: A): Stream[A] = new Stream[A] {
    private[this] var _hasNext = true

    def hasNext: Boolean = _hasNext

    def reset(): Unit = ()

    def next(): A = {
      if (!_hasNext) Stream.exhausted()
      _hasNext = false
      elem
    }
  }
}
abstract class Stream[A] { outer =>
  def reset(): Unit
  def hasNext: Boolean
  def next(): A

  def map[B](f: A => B): Stream[B] = new Stream[B] {
    def reset(): Unit = ()

    def hasNext: Boolean = outer.hasNext

    def next(): B = f(outer.next())
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = new Stream[B] {
    def reset(): Unit = ()

    private[this] var _hasNext: Boolean = _
    private[this] var sub: Stream[B] = _

    @tailrec
    private def step(): Unit = {
      _hasNext = outer.hasNext
      if (_hasNext) {
        sub = f(outer.next())
        _hasNext = sub.hasNext
        if (!_hasNext) step()
      }
    }

    step()

    def hasNext: Boolean = _hasNext

    def next(): B = {
      if (!_hasNext) Stream.exhausted()
      val res = sub.next()
      if (!sub.hasNext) step()
      res
    }
  }

  def zip[B](that: Stream[B]): Stream[(A, B)] = new Stream[(A, B)] {
    def reset(): Unit = ()

    def hasNext: Boolean = outer.hasNext && that.hasNext

    def next(): (A, B) = (outer.next(), that.next())
  }

  def ++ [B >: A](that: Stream[B]): Stream[B] = ???

  def take(n: Int): Stream[A] = new Stream[A] {
    private[this] var i = 0

    def reset(): Unit = ()

    def hasNext: Boolean = i < n && outer.hasNext

    def next(): A = {
      if (!hasNext) Stream.exhausted()
      i += 1
      outer.next()
    }
  }

  def drop(n: Int): Stream[A] = ???

  def isEmpty : Boolean = !hasNext
  def nonEmpty: Boolean = hasNext

  def toList: List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) b += next()
    b.result()
  }

  def size: Int = ???

  def foreach(fun: A => Unit): Unit =
    while (hasNext) fun(next())
}
