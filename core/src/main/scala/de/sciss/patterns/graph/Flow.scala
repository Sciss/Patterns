///*
// *  Flow.scala
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
//
//final case class Flow[A] private[patterns](in: Pat[A], level: Int) extends Pattern[A] {
//  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
//    logStream(s"Flow($in).iterator")
//    new StreamImpl[Tx](tx)
//  }
//
//  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
//    val inT = t(in)
//    if (inT.eq(in)) this else copy(in = inT)
//  }
//
//  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
//    private[this] val peer = in.expand(ctx, tx0)
//
//    def reset(levelR: Int)(implicit tx: Tx): Unit = {
//      logStream(s"Flow($in).iterator.reset()")
//      val gated = levelR < level
//      println(s"FLOW ${hashCode().toHexString} reset($levelR) -- my level $level -- $gated")
//      if (gated) peer.reset(levelR)
//    }
//
//    def hasNext(implicit tx: Tx): Boolean =
//      peer.hasNext
//
//    def next()(implicit tx: Tx): A = {
//      val res = peer.next()
//      logStream(s"Flow($in).iterator.next() = $res")
//      res
//    }
//  }
//}
