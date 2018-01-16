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

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  def fill[A](n: Int)(elem: => A): Stream[A] = ???

  def empty[A]: Stream[A] = new Stream[A] {
    def reset(): Unit = ()

    def hasNext: Boolean = false

    def next(): A = ???
  }

  def reverseIterator[A](that: Iterable[A]): Stream[A] = ???

  def continually[A](elem: => A): Stream[A] = new Stream[A] {
    def reset(): Unit = ()

    def hasNext: Boolean = true

    def next(): A = elem
  }

  def single[A](elem: A): Stream[A] = ???
}
trait Stream[A] { outer =>
  def reset(): Unit
  def hasNext: Boolean
  def next(): A

  def map[B](f: A => B): Stream[B] = new Stream[B] {
    def reset(): Unit = ()

    def hasNext: Boolean = outer.hasNext

    def next(): B = f(outer.next())
  }

  def flatMap[B](that: A => Stream[B]): Stream[B] = ???

  def zip[B](that: Stream[B]): Stream[(A, B)] = ???

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

  def foreach(fun: A => Unit): Unit = ???
}
