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

import de.sciss.patterns.Types.{Bridge, IntTop, Num, NumFrac, Top}
import de.sciss.patterns.graph._

final class PatOps[T <: Top](private val x: Pat[T]) extends AnyVal {
  def take(length: Pat[IntTop]): Take[T] = Take(x, length)
  def drop(length: Pat[IntTop]): Drop[T] = Drop(x, length)

  def head: Pat[T] = take(1)
  def tail: Pat[T] = drop(1)

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

  def linlin[T1 <: Top, T2 <: Top](inLo: Pat[T], inHi: Pat[T], outLo: Pat[T1], outHi: Pat[T1])
                                  (implicit br: Bridge[T, T1, T2], num: NumFrac[T2]): Pat[T2] =
    LinLin[T, T1, T2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def stutter(n: Pat.Int): Pat[T] = Stutter(n, x)

  def distinct: Pat[T] = Distinct(x)

  /** Same as `length`. */
  def size  : Pat.Int = Length(x)
  def length: Pat.Int = Length(x)

  def sorted(implicit ord: Ordering[T#Out]): Pat[T] = Sorted(x)

  def map[A <: Top, B <: Top](f: Pat[A] => Pat[B])(implicit ev: T <:< Pat[A]): Pat[Pat[B]] = {
    val token = Graph.builder.allocToken()
    val it    = It[A](token)
    val inner = Graph {
      f(it)
    }
    PatMap(x.asInstanceOf[Pat[Pat[A]]], it, inner)
  }

  def flatMap  [A <: Top, B <: Top](f:     T  => Pat[B])(implicit ev: T <:< Pat[A]): Pat[B]       = ???
  def bubbleMap[          B <: Top](f: Pat[T] => Pat[B])                           : Pat[B]       = ???

  def flatten[A <: Top](implicit ev: T <:< Pat[A]): Pat[A] = Flatten(x.asInstanceOf[Pat[Pat[A]]])

  def combinations(n: Pat.Int): Pat[Pat[T]] = Combinations(x, n)
}