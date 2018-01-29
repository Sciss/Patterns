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
import de.sciss.patterns.graph.It

import scala.collection.immutable.{IndexedSeq => Vec}

object Graph {
  trait Builder {
    def addPattern(p: Pattern[_]): Unit

    def allocToken[T <: Top](): It[T]

    def isOutside: Boolean

    def visit[P](ref: AnyRef, init: => P): P
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder  = builderRef.get()

  private[this] val builderRef = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    def isOutside = true

    def addPattern(p: Pattern[_]): Unit = ()

    def allocToken[T <: Top](): It[T] = outOfContext

    def visit[P](ref: AnyRef, init: => P): P = outOfContext
  }

  def apply[T <: Top](thunk: => Pat[T]): Graph[T] = {
    val old = builderRef.get()
    val b   = new BuilderImpl[T](old)
    builderRef.set(b)
    try {
      val out = thunk
      b.build(out)
    } finally {
      builderRef.set(old) // BuilderDummy
    }
  }

  private[this] final class BuilderImpl[T <: Top](val parent: Builder) extends Builder {
    private[this] val lazies    = Vec.newBuilder[Pattern[_]]
    private[this] var sourceMap = Map.empty[AnyRef, Any]
    private[this] var tokenId   = 0

    def isOutside = false

    override def toString = s"patterns.Graph.Builder@${hashCode.toHexString}"

    def build(out: Pat[T]) = Graph(lazies.result(), out)

    def addPattern(p: Pattern[_]): Unit = lazies += p

    def allocToken[U <: Top](): It[U] =
      if (parent.isOutside) {
        val res = tokenId
        tokenId += 1
        It(res)
      } else {
        parent.allocToken()
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

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
//    private[this] val sourceStreams = sources.map(_.expand)
    private[this] val peer = out.expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit =
      sources.foreach(_.reset())

    def hasNext(implicit tx: Tx): Boolean   = peer.hasNext
    def next ()(implicit tx: Tx): T#Out[Tx] = peer.next()
  }
}
