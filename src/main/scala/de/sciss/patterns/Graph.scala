package de.sciss.patterns

import scala.collection.immutable.{IndexedSeq => Vec}

object Graph {
  trait Builder {
    def addLazy(x: Lazy): Unit
  }

  /** This is analogous to `SynthGraph.Builder` in ScalaCollider. */
  def builder: Builder  = builderRef.get()

  private[this] val builderRef = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  private[this] object BuilderDummy extends Builder {
    def addLazy(x: Lazy): Unit = ()
  }

  def apply(thunk: => Any): Graph = {
    val b   = new BuilderImpl
    val old = builderRef.get()
    builderRef.set(b)
    try {
      thunk
      b.build
    } finally {
      builderRef.set(old) // BuilderDummy
    }
  }

  private[this] final class BuilderImpl extends Builder {
    private val lazies = Vec.newBuilder[Lazy]

    override def toString = s"pattern.Graph.Builder@${hashCode.toHexString}"

    def build = Graph(lazies.result())

    def addLazy(g: Lazy): Unit = lazies += g
  }
}

final case class Graph(sources: Vec[Lazy] /* , controlProxies: Set[ControlProxyLike] */) {
  def isEmpty : Boolean  = sources.isEmpty // && controlProxies.isEmpty
  def nonEmpty: Boolean  = !isEmpty

  // def expand(implicit ctrl: stream.Control): UGenGraph = UGenGraph.build(this)
}