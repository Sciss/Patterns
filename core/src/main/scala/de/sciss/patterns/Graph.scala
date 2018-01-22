/*
 *  Graph.scala
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

import de.sciss.patterns.Types.Top

import scala.collection.immutable.{IndexedSeq => Vec}

object Graph {
  trait Builder {
    def addPattern(p: Pattern[_]): Unit

    def allocToken(): Int

    def visit[P](ref: AnyRef, init: => P): P
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder  = builderRef.get()

  private[this] val builderRef = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    def addPattern(p: Pattern[_]): Unit = ()

    def allocToken(): Int = -1

    def visit[P](ref: AnyRef, init: => P): P = outOfContext
  }

  def apply[T <: Top](thunk: => Pat[T]): Graph[T] = {
    val b   = new BuilderImpl[T]
    val old = builderRef.get()
    builderRef.set(b)
    try {
      val out = thunk
      b.build(out)
    } finally {
      builderRef.set(old) // BuilderDummy
    }
  }

  private[this] final class BuilderImpl[T <: Top] extends Builder {
    private[this] val lazies    = Vec.newBuilder[Pattern[_]]
    private[this] var sourceMap = Map.empty[AnyRef, Any]
    private[this] var tokenId   = 0

    override def toString = s"patterns.Graph.Builder@${hashCode.toHexString}"

    def build(out: Pat[T]) = Graph(lazies.result(), out)

    def addPattern(p: Pattern[_]): Unit = lazies += p

    def allocToken(): Int = {
      val res = tokenId
      tokenId += 1
      res
    }

    def visit[P](ref: AnyRef, init: => P): P = {
      // log(s"visit  ${ref.hashCode.toHexString}")
      sourceMap.getOrElse(ref, {
        // log(s"expand ${ref.hashCode.toHexString}...")
        val exp    = init
        // log(s"...${ref.hashCode.toHexString} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        sourceMap += ref -> exp
        exp
      }).asInstanceOf[P] // not so pretty...
    }
  }
}

final case class Graph[T <: Top](sources: Vec[Pattern[_]], out: Pat[T]) extends Pattern[T] {
  def isEmpty : Boolean = sources.isEmpty // && controlProxies.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = new Stream[Tx, T#Out[Tx]] {
//    private[this] val sourceStreams = sources.map(_.expand)
    private[this] val peer = out.expand

    def reset()(implicit tx: Tx): Unit =
      sources.foreach(_.reset())

    def hasNext(implicit tx: Tx): Boolean     = peer.hasNext
    def next ()(implicit tx: Tx): T#Out[Tx]  = peer.next()
  }
}
