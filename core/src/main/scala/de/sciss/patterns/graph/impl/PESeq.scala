///*
// *  PESeq.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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
//package impl
//
//import de.sciss.patterns.PE.Value
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//
//final case class PESeq[A <: Value](elems: Vec[PE[A]]) extends PE[A] {
//  private[patterns] def expand(implicit b: StreamGraph.Builder): Stream[A] =
//    ??? // StreamInGroup(elems.map(_.expand))
//
//  def toStream(implicit b: StreamGraph.Builder): Stream[A] = ???
//
//  override def toString: String = elems.mkString("PESeq(", ",", ")")
//}