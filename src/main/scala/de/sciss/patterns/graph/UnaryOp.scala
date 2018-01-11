///*
// *  UnaryOp.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
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
//import de.sciss.patterns.Types.{IntTop, Num, ScalarTop, Top}
//
//object UnaryOp {
//  sealed abstract class Op[T1 <: Top, T <: Top] {
//    def apply(a: T1#Out): T#Out
//  }
//
//  final case class ToInt[T <: ScalarTop]()(implicit num: Num[T]) extends Op[T, IntTop] {
//    def apply(a: T#Out): Int = ??? // num.abs()
//  }
//}
//final case class UnaryOp[T1 <: Top, T <: Top](op: UnaryOp.Op[T1, T], a: Pat[T1])
//  extends Pattern[T] {
//
//  def iterator(implicit ctx: Context): Iterator[T#Out] = {
//    val ai = a.expand
//    ai.map { av => op(av) }
//  }
//}
