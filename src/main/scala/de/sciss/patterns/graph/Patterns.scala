///*
// *  Patterns.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.patterns
//package graph
//
//import de.sciss.patterns.PE.Value
//
///** aka Pseries */
//final case class ArithmSeq[A <: PE.Numeric](start: PE[A], step: PE[A], length: PE.Int = Int.MaxValue)
//                                           (implicit num: scala.Numeric[A#Out])
//  extends Pattern[A] {
//
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] =
//    new stream.ArithmSeq(start = start.expand, step = step.expand, length = length.expand)
//}
//
///** aka Pgeom */
//final case class GeomSeq[A <: PE.Numeric](start: PE[A], grow: PE[A], length: PE.Int = Int.MaxValue)
//  extends Pattern[A] {
//
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] =
//    new stream.GeomSeq(start = start.expand, grow = grow.expand, length = length.expand)
//}