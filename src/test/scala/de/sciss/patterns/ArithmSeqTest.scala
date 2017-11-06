package de.sciss.patterns

import de.sciss.patterns.graph.ArithmSeq

object ArithmSeqTest extends App {
  implicit val b: StreamGraph.Builder = null

  val a = ArithmSeq(300, 20, 70).toStream
  val it = a.iterator
  it.foreach { value =>
    println(value)
  }
}
