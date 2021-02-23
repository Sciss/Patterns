/*
 *  Graph.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.graph.{It, Pat}

object Graph {
  trait Builder {
    def allocToken[A](): It[A]

    def isOutside: Boolean
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder = builderRef.get()

  private[this] val builderRef = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    private def outOfContext: Nothing = sys.error("Out of context")

    final val isOutside = true

    def allocToken[A](): It[A] = outOfContext
  }

  def apply[A](thunk: => Pat[A]): Pat[A] /* Graph[A] */ = {
    val old = builderRef.get()
    val b   = new BuilderImpl[A](old)
    builderRef.set(b)
    try {
      val out = thunk
      out
    } finally {
      builderRef.set(old)
    }
  }

  private[this] final class BuilderImpl[A](val parent: Builder) extends Builder {
    private[this] var tokenId   = 0

    def isOutside : Boolean = false

    override def toString = s"patterns.Graph.Builder@${hashCode.toHexString}"

    def allocToken[U](): It[U] =
      if (parent.isOutside) {
        val res = tokenId
        tokenId += 1
        It(res)
      } else {
        parent.allocToken()
      }
  }
}

