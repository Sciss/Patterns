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

import de.sciss.patterns.Types.{Bridge, IntTop, Num, NumFrac, Ord, Top}
import de.sciss.patterns.graph._

final class PatOps[T <: Top](private val x: Pat[T]) extends AnyVal {
  def take(length: Pat[IntTop]): Pat[T] = Take(x, length)
  def drop(length: Pat[IntTop]): Pat[T] = Drop(x, length)

  def head: Pat[T] = take(1)
  def tail: Pat[T] = drop(1)

  def ++[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2]): Pat[T2] /* Cat[T, T1, T2] */ =
    Cat(x, that)

  def + [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Plus[T2]()
    BinaryOp(op, x, that)
  }

  def * [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Times[T2]()
    BinaryOp(op, x, that)
  }

  def roundTo[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.RoundTo[T2]()
    BinaryOp(op, x, that)
  }

  def <  [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Lt[T2]()
    BinaryOp(op, x, that)
  }

  def <= [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Leq[T2]()
    BinaryOp(op, x, that)
  }

  def >  [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Gt[T2]()
    BinaryOp(op, x, that)
  }

  def >= [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Bridge[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Geq[T2]()
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

  def sorted(implicit ord: Ord[T]): Pat[T] = Sorted(x)

  /** Short-cut for `grouped(1)`. For example,
    * `Pat(1, 2, 3)` becomes `Pat(Pat(1), Pat(2), Pat(3))`.
    */
  def bubble: Pat[Pat[T]] = grouped(1)

  def grouped(size: Pat.Int): Pat[Pat[T]] = Grouped(x, size)

  /** Similar to a monadic `map` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def map[A <: Top, B <: Top](f: Pat[A] => Pat[B])(implicit ev: T <:< Pat[A]): Pat[Pat[B]] = {
    val it    = Graph.builder.allocToken[A]()
    val inner = Graph {
      f(it)
    }
    PatMap(x.asInstanceOf[Pat[Pat[A]]], it, inner)
  }

  /** Similar to a monadic `flatMap` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def flatMap[A <: Top, B <: Top](f: Pat[A] => Pat[B])(implicit ev: T <:< Pat[A]): Pat[B] = {
    val it    = Graph.builder.allocToken[A]()
    val inner = Graph {
      f(it)
    }
    FlatMap(x.asInstanceOf[Pat[Pat[A]]], it, inner)
  }

  def filter[A <: Top](f: Pat[A] => Pat.Boolean)(implicit ev: T <:< Pat[A]): Pat[Pat[A]] = {
    val it    = Graph.builder.allocToken[A]()
    val inner = Graph {
      f(it)
    }
    Filter(x.asInstanceOf[Pat[Pat[A]]], it, inner)
  }

  def bubbleFilter[A <: Top](f: Pat[T] => Pat.Boolean): Pat[T] =
    bubble.filter(f).flatten

  def indexOfSlice[A <: Top](that: Pat[A]): Pat.Int = indexOfSlice(that, 0)

  /** Finds first index after or at a start index where this pattern
    * contains a given other pattern as a slice.
    *
    *  @param  that    the sequence to test
    *  @param  from    the start index
    *  @return  the first index `>= from` such that the elements of this pattern starting at this index
    *           match the elements of pattern `that`, or `-1` of no such subsequence exists.
    */
  def indexOfSlice[A <: Top](that: Pat[A], from: Pat.Int): Pat.Int =
    IndexOfSlice(x, that, from)

//  /** currently broken
//    *
//    * The "counter" operation to `flatMap`. It bubbles each
//    * element of the receiver pattern as a singleton pattern itself,
//    * then applies the function, and flattens the result.
//    *
//    * This way, we can translate `p.flatMap(_.map)` into
//    * `p.map(_.bubbleMap)` when `T` is not a (nested) pattern.
//    * For example an operation on collections
//    *
//    * {{{
//    *   a.flatMap { v => b.map { w => v :+ w }}
//    * }}}
//    *
//    * can be rewritten for patterns as
//    *
//    * {{{
//    *   a.map { v: Pat[A] => b.bubbleMap { w => v ++ w }}
//    * }}}
//    */
//  def bubbleMap[A <: Top](f: Pat[T] => Pat[A]): Pat[A] = {
//    val it    = Graph.builder.allocToken[T]()
//    val inner = Graph {
//      f(it)
//    }
//    BubbleMap[T, A](x, it, inner)
//  }

  /** Short-hand for `.bubble.map.flatten` */
  def bubbleMap[A <: Top](f: Pat[T] => Pat[A]): Pat[A] =
    bubble.map(f).flatten

  def flatten[A <: Top](implicit ev: T <:< Pat[A]): Pat[A] = Flatten(x.asInstanceOf[Pat[Pat[A]]])

  def combinations(n: Pat.Int): Pat[Pat[T]] = Combinations(x, n)

  def recur(): Pat[T] = Recur(x)
}