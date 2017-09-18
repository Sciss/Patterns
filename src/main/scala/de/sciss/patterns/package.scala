package de.sciss

import org.coroutines._

import scala.language.implicitConversions

package object patterns {
  type Stream[A] = Iterator[A]

  implicit def lift[A](co: Coroutine._0[A, _]): Stream[A] = new CoroutineIterator[A](call(co()))
  implicit def lift[A](co: ~~~>[A, _]): Stream[A] = new CoroutineIterator[A](call(co()))
}