/*
 *  PatOps.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.Types.{Bridge, IntTop, Num, Top, TopT}
import de.sciss.patterns.graph._

final class PatOps[T <: Top](private val x: Pat[T]) extends AnyVal {
  def take(length: Pat[IntTop]): Take[T] = Take(x, length)
  def drop(length: Pat[IntTop]): Drop[T] = Drop(x, length)

  def ++[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2]): Cat[T, T1, T2] = Cat(x, that)

  def + [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): BinaryOp[T, T1, T2] = {
    val op = BinaryOp.Plus[T2]()
    BinaryOp(op, x, that)
  }

  def * [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): BinaryOp[T, T1, T2] = {
    val op = BinaryOp.Times[T2]()
    BinaryOp(op, x, that)
  }

  def roundTo[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): BinaryOp[T, T1, T2] = {
    val op = BinaryOp.RoundTo[T2]()
    BinaryOp(op, x, that)
  }

  def stutter(n: Pat.Int): Pat[T] = Stutter(n, x)

  def distinct: Pat[T] = ???

  def size: Pat.Int = ???

  def sorted(implicit ord: Ordering[T#Out]): Pat[T] = ???

  def combinations(n: Pat.Int): Pat[TopT[Seq[T#Out]]] = ???
}