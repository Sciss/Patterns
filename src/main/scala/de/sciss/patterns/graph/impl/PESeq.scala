package de.sciss.patterns
package graph
package impl

import scala.collection.immutable.{IndexedSeq => Vec}

final case class PESeq(elems: Vec[PE]) extends PE {
  private[patterns] def expand(implicit b: StreamGraph.Builder): StreamInLike =
    StreamInGroup(elems.map(_.expand))

  override def toString: String = elems.mkString("PESeq(", ",", ")")
}