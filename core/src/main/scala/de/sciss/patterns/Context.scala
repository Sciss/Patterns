/*
 *  Context.scala
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

import de.sciss.lucre.stm.{Sink, Source}
import de.sciss.patterns.Context.Var

import scala.util.Random

trait Context[Tx] {
//  def visit[U](ref: AnyRef, init: => U): U

  def addStream[A](ref: AnyRef, stream: Stream[Tx, A]): Stream[Tx, A]

  def getStreams(ref: AnyRef): List[Stream[Tx, _]]

  def getOuterStream[A](token: Int): Stream[Tx, A]

  def setOuterStream[A](token: Int, outer: Stream[Tx, A]): Unit

  def mkRandom(): Random

  def newVar[A](init: A): Var[Tx, A]
//  def newVar[A](init: A): Var[A]
//  def newIntVar(init: Int)(implicit tx: Tx): Var[Int]
}

object Context {
  def apply(): Plain = new PlainImpl

  type Var[Tx, A] = Sink[Tx, A] with Source[Tx, A]

  trait Plain extends Context[Unit] {
    implicit val tx: Unit = ()
  }

  private abstract class Impl[Tx] extends ContextLike[Tx] {
    private[this] lazy val seedRnd = new Random()
    private[this] var tokenMap = Map.empty[Int, Stream[Tx, _]]

    def setOuterStream[A](token: Int, outer: Stream[Tx, A]): Unit =
      tokenMap += token -> outer

    def getOuterStream[A](token: Int): Stream[Tx, A] = tokenMap(token).asInstanceOf[Stream[Tx, A]]

    def mkRandom(): Random = new Random(seedRnd.nextLong())
  }

  private final class PlainImpl extends Impl[Unit] with Plain {
    def newVar[A](init: A): Var[Unit, A] = new PlainVar[A](init)
  }

  private final class PlainVar[A](private[this] var current: A) extends Sink[Unit, A] with Source[Unit, A] {
    def apply()(implicit tx: Unit): A =
      current

    def update(v: A)(implicit tx: Unit): Unit =
      current = v
  }
}

private[patterns] abstract class ContextLike[Tx] extends Context[Tx] {
  private[this] var streamMap = Map.empty[AnyRef, List[Stream[Tx, _]]]

  def addStream[A](ref: AnyRef, stream: Stream[Tx, A]): Stream[Tx, A] = {
    streamMap += ref -> (stream :: streamMap.getOrElse(ref, Nil))
    stream
  }

  def getStreams(ref: AnyRef): List[Stream[Tx, _]] = streamMap.getOrElse(ref, Nil)
}