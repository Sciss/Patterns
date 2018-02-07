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

import de.sciss.patterns.Types.{Widen, Num, NumFrac, Ord, Top, Tuple2Top}
import de.sciss.patterns.graph._

final class PatOps[T <: Top](private val x: Pat[T]) extends AnyVal {
  def take(length: Pat.Int): Pat[T] = Take(x, length)
  def drop(length: Pat.Int): Pat[T] = Drop(x, length)

  def head: Pat[T] = take(1)
  def tail: Pat[T] = drop(1)

  def splitAt(index: Pat.Int): (Pat[T], Pat[T]) = (take(index), drop(index))

  // unary

  def differentiate(implicit num: Num[T]): Pat[T] = Differentiate(x)

  def sum(implicit num: Num[T]): Pat[T] = Sum(x)

  // binary

  def ++[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2]): Pat[T2] /* Cat[T, T1, T2] */ =
    Cat(x, that)

  def + [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Plus[T2]()
    BinaryOp(op, x, that)
  }

  def - [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Minus[T2]()
    BinaryOp(op, x, that)
  }

  def * [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Times[T2]()
    BinaryOp(op, x, that)
  }

  def % [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.%[T2]()
    BinaryOp(op, x, that)
  }

  def mod[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.Mod[T2]()
    BinaryOp(op, x, that)
  }

  def roundTo[T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], num: Num[T2]): Pat[T2] = {
    val op = BinaryOp.RoundTo[T2]()
    BinaryOp(op, x, that)
  }

  def <  [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Lt[T2]()
    BinaryOp(op, x, that)
  }

  def <= [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Leq[T2]()
    BinaryOp(op, x, that)
  }

  def >  [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Gt[T2]()
    BinaryOp(op, x, that)
  }

  def >= [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2], ord: Ord[T2]): Pat.Boolean = {
    val op = BinaryOp.Geq[T2]()
    BinaryOp(op, x, that)
  }

  def sig_== [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2]): Pat.Boolean = {
    val op = BinaryOp.Eq[T2]()
    BinaryOp(op, x, that)
  }

  def sig_!= [T1 <: Top, T2 <: Top](that: Pat[T1])(implicit br: Widen[T, T1, T2]): Pat.Boolean = {
    val op = BinaryOp.Neq[T2]()
    BinaryOp(op, x, that)
  }

  def linlin[T1 <: Top, T2 <: Top](inLo: Pat[T], inHi: Pat[T], outLo: Pat[T1], outHi: Pat[T1])
                                  (implicit br: Widen[T, T1, T2], num: NumFrac[T2]): Pat[T2] =
    LinLin[T, T1, T2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def stutter(n: Pat.Int): Pat[T] = Stutter(n, x)

  def distinct: Pat[T] = Distinct(x)

  /** Same as `length`. */
  def size    : Pat.Int = Length(x)
  def length  : Pat.Int = Length(x)

  def indices : Pat.Int = Indices(x)

  def sorted(implicit ord: Ord[T]): Pat[T] = Sorted(x)

  /** Short-cut for `grouped(1)`. For example,
    * `Pat(1, 2, 3)` becomes `Pat(Pat(1), Pat(2), Pat(3))`.
    */
  def bubble: Pat[Pat[T]] = grouped(1)

  def grouped(size: Pat.Int): Pat[Pat[T]] = Grouped(x, size)

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

  def sliding(size: Pat.Int): Pat[Pat[T]] = sliding(size, step = 1)

  def sliding(size: Pat.Int, step: Pat.Int): Pat[Pat[T]] = Sliding(x, size = size, step = step)

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

  def combinations(n: Pat.Int): Pat[Pat[T]] = Combinations(x, n)

  def recur(): Pat[T] = Recur(x)

  def zip[A <: Top](that: Pat[A]): Pat.Tuple2[T, A] = Zip2(x, that)

  def unzip[A <: Top, B <: Top](implicit ev: T <:< Tuple2Top[A, B]): (Pat[A], Pat[B]) = {
    val tup = x.asInstanceOf[Pat.Tuple2[A, B]]
    (Tuple2_1(tup), Tuple2_2(tup))
  }

  def poll(label: Pat.String = "poll", gate: Pat.Boolean = true): Pat[T] =
    Poll(x, gate = gate, label = label)

  /** "Taps" into this pattern by appending a side-effect. The returned
    * pattern will invoke both the input stream and the stream
    * produced from the side-effecting pattern produced by `f`,
    * and then pass through the input stream's value.
    * The side-effecting stream may end early, the compound stream keeps
    * producing while the input stream has elements.
    *
    * Similar to `runWith` for standard Scala collections.
    */
  def <| [A <: Top](f: Pat[T] => Pat[A]): Pat[T] = Tap(x, f(x))
}

final class PatNestedOps[T <: Top](private val x: Pat[Pat[T]]) extends AnyVal {
  /** Similar to a monadic `map` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def map[B <: Top](f: Pat[T] => Pat[B]): Pat[Pat[B]] = {
    val it    = Graph.builder.allocToken[T]()
    val inner = Graph {
      f(it)
    }
    PatMap(x, it, inner)
  }

  /** Similar to a monadic `flatMap` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def flatMap[B <: Top](f: Pat[T] => Pat[B]): Pat[B] = {
    val it    = Graph.builder.allocToken[T]()
    val inner = Graph {
      f(it)
    }
    FlatMap(x, it, inner)
  }

  def flatten: Pat[T] = Flatten(x)

  def filter(f: Pat[T] => Pat.Boolean): Pat[Pat[T]] = {
    val it    = Graph.builder.allocToken[T]()
    val inner = Graph {
      f(it)
    }
    Filter(x, it, inner)
  }

  // def /: [B <: Top](z: Pat[B])(op: (Pat[B], Pat[T]) => Pat[B]): Pat[B] = foldLeft(z)(op)

  def foldLeft[B <: Top](z: Pat[B])(op: (Pat[B], Pat[T]) => Pat[B]): Pat[B] = {
    val b     = Graph.builder
    val it    = b.allocToken[Tuple2Top[T, B]]()
    val inner = Graph {
      val (itT, itB) = it.unzip
      op(itB, itT)
    }
    FoldLeft[T, B](x, z, it, inner)
  }

  def sortWith(lt: (Pat[T], Pat[T]) => Pat.Boolean): Pat[Pat[T]] = {
    val b     = Graph.builder
    val it    = b.allocToken[Tuple2Top[T, T]]()
    val inner = Graph {
      val (it1, it2) = it.unzip
      lt(it1, it2)
    }
    SortWith(x, it, inner)
  }
}