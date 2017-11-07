package de.sciss.patterns

import de.sciss.patterns.graph.{ArithmSeq, GeomSeq}

object ArithmSeqTest extends App {
  implicit val builder: StreamGraph.Builder = null

//  val a = ArithmSeq(300, 20, 70).toStream
//  a.iterator.foreach { value =>
//    println(value)
//  }
//
//  val b = GeomSeq(1.0, 1.1).toStream
//  b.iterator.take(100).foreach(println)

  val c = ArithmSeq(Seq(300, 301), 20, 70).toStream
  c.iterator.foreach { value =>
    println(value)
  }
}
