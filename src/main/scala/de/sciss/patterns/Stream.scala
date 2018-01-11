/*
 *  Stream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

object Stream {
  def fill[A](n: Int)(elem: => A): Stream[A] = ???

  def empty[A]: Stream[A] = ???

  def reverseIterator[A](that: Iterable[A]): Stream[A] = ???

  def continually[A](elem: => A): Stream[A] = ???

  def single[A](elem: A): Stream[A] = ???
}
trait Stream[A] {
  def hasNext: Boolean
  def reset(): Unit
  def next(): A

  def map[B](that: A => B): Stream[B] = ???

  def flatMap[B](that: A => Stream[B]): Stream[B] = ???

  def zip[B](that: Stream[B]): Stream[(A, B)] = ???

  def ++ [B >: A](that: Stream[B]): Stream[B] = ???

  def take(n: Int): Stream[A] = ???
  def drop(n: Int): Stream[A] = ???

  def isEmpty : Boolean = !hasNext
  def nonEmpty: Boolean = hasNext

  def toList: List[A] = ???

  def size: Int = ???

  def foreach(fun: A => Unit): Unit = ???
}
