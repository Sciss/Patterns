///*
// *  FoldLeftCarryStream.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.patterns
//package graph
//package impl
//
//final class FoldLeftCarryStream[Tx, A](level: Int, fold: FoldLeft[_, A])
//  extends Stream[Tx, A] { id =>
//
//  val innerStream = useLevel(level - 1)(inner.expand)
//
//  def hasNext: Boolean = useLevel(level -1) {
//    innerStream.hasNext
//  }
//
//  def next(): A = useLevel(level - 1) {
//    innerStream.next()
//  }
//
//  def reset()(implicit tx: Tx): Unit = {
//    println("FoldLeftInStream. TODO: reset")
//    ()
//  }
//
//  def hasNext(implicit tx: Tx): Boolean =
//    buf.hasNext(id)
//
//  def next()(implicit tx: Tx): A =
//    buf.next(id)
//}
