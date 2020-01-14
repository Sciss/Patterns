/*
 *  Graph.scala
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

import de.sciss.patterns.graph.It

object Graph {
  trait Builder {
//    def addPattern(p: Pattern[_]): Unit

    def allocToken[A](): It[A]

    def isOutside: Boolean

//    def level: Int

//    def visit[P](ref: AnyRef, init: => P): P
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder = builderRef.get()

  private[this] val builderRef = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    final val isOutside = true
//    final val level     = 0

//    def addPattern(p: Pattern[_]): Unit = ()

    def allocToken[A](): It[A] = outOfContext

//    def visit[P](ref: AnyRef, init: => P): P = outOfContext
  }

  def apply[A](thunk: => Pat[A]): Pat[A] /* Graph[A] */ = {
    val old = builderRef.get()
    val b   = new BuilderImpl[A](old)
    builderRef.set(b)
    try {
      val out = thunk
      out // b.build(out)
    } finally {
      builderRef.set(old) // BuilderDummy
    }
  }

  private[this] final class BuilderImpl[A](val parent: Builder) extends Builder {
//    private[this] val lazies    = Vec.newBuilder[Pattern[_]]
//    private[this] var sourceMap = Map.empty[AnyRef, Any]
    private[this] var tokenId   = 0

    def isOutside : Boolean = false
//    val level     : Int     = parent.level + 1

    // $COVERAGE-OFF$
    override def toString = s"patterns.Graph.Builder@${hashCode.toHexString}"
    // $COVERAGE-ON$

//    def build(out: Pat[A]): Graph[A] = Graph(lazies.result(), out)

//    def addPattern(p: Pattern[_]): Unit = lazies += p

    def allocToken[U](): It[U] =
      if (parent.isOutside) {
        val res = tokenId
        tokenId += 1
        It(res)
      } else {
        parent.allocToken()
      }

//    def visit[P](ref: AnyRef, init: => P): P = {
//      // log(s"visit  ${ref.hashCode.toHexString}")
//      sourceMap.getOrElse(ref, {
//        // log(s"expand ${ref.hashCode.toHexString}...")
//        val exp    = init
//        // log(s"...${ref.hashCode.toHexString} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
//        sourceMap += ref -> exp
//        exp
//      }).asInstanceOf[P] // not so pretty...
//    }
  }
}

//final case class Graph[A](sources: Vec[Pat[_]], out: Pat[A]) extends Pattern[A] {
//  def isEmpty : Boolean = sources.isEmpty // && controlProxies.isEmpty
//  def nonEmpty: Boolean = !isEmpty
//
//  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)
//
//  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
//    val sourcesT = sources.map(t(_))
//    val outT     = t(out)
//    copy(sources = sourcesT, out = outT)
//  }
//
//  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
//
//    private[this] val peer = out.expand(ctx, tx0)
//
//    def reset()(implicit tx: S#Tx): Unit = {
////      sources.foreach(_.reset())
//      peer.reset()
//    }
//
//    def hasNext(implicit tx: S#Tx): Boolean = peer.hasNext
//    def next ()(implicit tx: S#Tx): A       = peer.next()
//  }
//}
