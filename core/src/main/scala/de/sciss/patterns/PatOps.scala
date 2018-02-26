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

import de.sciss.patterns.Types.{Num, NumFrac, Ord, Widen}
import de.sciss.patterns.graph._

final class PatOps[A](private val x: Pat[A]) extends AnyVal {
  def take(length: Pat[Int] = 1): Pat[A] = Take(x, length)
  def drop(length: Pat[Int]    ): Pat[A] = Drop(x, length)

//  def head: Pat[A] = take(1)
  def tail: Pat[A] = drop(1)

  def splitAt(index: Pat[Int]): (Pat[A], Pat[A]) = (take(index), drop(index))

  // unary

  def differentiate(implicit num: Num[A]): Pat[A] = Differentiate(x)

  def sum(implicit num: Num[A]): Pat[A] = Sum(x)

  // binary

  def ++[A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2]): Pat[A2] =
    Cat(x, that)

  def + [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.Plus[A2]()
    BinaryOp(op, x, that)
  }

  def - [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.Minus[A2]()
    BinaryOp(op, x, that)
  }

  def * [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.Times[A2]()
    BinaryOp(op, x, that)
  }

  def % [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.%[A2]()
    BinaryOp(op, x, that)
  }

  def mod[A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.Mod[A2]()
    BinaryOp(op, x, that)
  }

  def roundTo[A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinaryOp.RoundTo[A2]()
    BinaryOp(op, x, that)
  }

  def <  [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinaryOp.Lt[A2]()
    BinaryOp(op, x, that)
  }

  def <= [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinaryOp.Leq[A2]()
    BinaryOp(op, x, that)
  }

  def >  [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinaryOp.Gt[A2]()
    BinaryOp(op, x, that)
  }

  def >= [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinaryOp.Geq[A2]()
    BinaryOp(op, x, that)
  }

  def sig_== [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2]): Pat[Boolean] = {
    val op = BinaryOp.Eq[A2]()
    BinaryOp(op, x, that)
  }

  def sig_!= [A1, A2](that: Pat[A1])(implicit br: Widen[A, A1, A2]): Pat[Boolean] = {
    val op = BinaryOp.Neq[A2]()
    BinaryOp(op, x, that)
  }

  def linlin[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit br: Widen[A, A1, A2], num: NumFrac[A2]): Pat[A2] =
    LinLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def stutter(n: Pat[Int]): Pat[A] = Stutter(x, n)

  def distinct: Pat[A] = Distinct(x)

  /** Same as `length`. */
  def size    : Pat[Int]  = Length(x)
  def length  : Pat[Int]  = Length(x)

  def indices : Pat[Int]  = Indices(x)

  def sorted(implicit ord: Ord[A]): Pat[A] = Sorted(x)

  /** Short-cut for `grouped(1)`. For example,
    * `Pat(1, 2, 3)` becomes `Pat(Pat(1), Pat(2), Pat(3))`.
    */
  def bubble: Pat[Pat[A]] = grouped(1)

  def grouped(size: Pat[Int]): Pat[Pat[A]] = Grouped(x, size)

//  def bubbleFilter(f: Pat[A] => Pat[Boolean]): Pat[A] =
//    bubble.filter(f).flatten

  def indexOfSlice[B](that: Pat[B]): Pat[Int] = indexOfSlice(that, 0)

  /** Finds first index after or at a start index where this pattern
    * contains a given other pattern as a slice.
    *
    *  @param  that    the sequence to test
    *  @param  from    the start index
    *  @return  the first index `>= from` such that the elements of this pattern starting at this index
    *           match the elements of pattern `that`, or `-1` of no such subsequence exists.
    */
  def indexOfSlice[B](that: Pat[B], from: Pat[Int]): Pat[Int] =
    IndexOfSlice(x, that, from)

  def sliding(size: Pat[Int]): Pat[Pat[A]] = sliding(size, step = 1)

  def sliding(size: Pat[Int], step: Pat[Int]): Pat[Pat[A]] = Sliding(x, size = size, step = step)

  /** Short-hand for `.bubble.map.flatten` */
  def bubbleMap(f: Pat[A] => Pat[A]): Pat[A] =
    bubble.map(f).flatten

  def combinations(n: Pat[Int]): Pat[Pat[A]] = Combinations(x, n)

//  def recur(): Pat[A] = Recur(x)

//  def flow(): Pat[A] = {
//    val level = Graph.builder.level
//    Flow(x, level = level)
//  }

  def hold(): Pat[A] = Hold(x)

  def loop(n: Pat[Int] = Int.MaxValue): Pat[A] = Pat.loop(n)(x)

  def zip[B](that: Pat[B]): Pat[(A, B)] = Zip2(x, that)

  def poll(label: Pat[String] = "poll", gate: Pat[Boolean] = true): Pat[A] =
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
  def <| [B](f: Pat[A] => Pat[B]): Pat[A] = Tap(x, f(x))
}

final class PatNestedOps[A](private val x: Pat[Pat[A]]) extends AnyVal {
  /** Similar to a monadic `map` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def map[B](f: Pat[A] => Pat[B]): Pat[Pat[B]] = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
//    val level = b.level + 1
    val inner = Graph {
      f(it)
    }
    PatMap(outer = x, it = it, inner = inner /* , innerLevel = level */)
  }

  def mapWithIndex[B](f: (Pat[A], Pat[Int]) => Pat[B]): Pat[Pat[B]] = {
    val b     = Graph.builder
    val itIn  = b.allocToken[A  ]()
    val itIdx = b.allocToken[Int]()
    val inner = Graph {
      f(itIn, itIdx)
    }
    MapWithIndex(outer = x, itIn = itIn, itIdx = itIdx, inner = inner)
  }

    /** Similar to a monadic `flatMap` but with the constraint
    * the element type must be a (nested) pattern.
    */
  def flatMap[B](f: Pat[A] => Pat[B]): Pat[B] = {
    val b     = Graph.builder
    val it    = b.allocToken[A]()
    val level = b.level + 1
    val inner = Graph {
      f(it)
    }
    FlatMap(outer = x, it = it, inner = inner, innerLevel = level)
  }

  def flatten: Pat[A] = Flatten(x)

//  def filter(f: Pat[A] => Pat[Boolean]): Pat[Pat[A]] = {
//    val it    = Graph.builder.allocToken[A]()
//    val inner = Graph {
//      f(it)
//    }
//    Filter(x, it, inner)
//  }

  // def /: [B <: Top](z: Pat[B])(op: (Pat[B], Pat[T]) => Pat[B]): Pat[B] = foldLeft(z)(op)

  def foldLeft[B](z: Pat[B])(op: (Pat[B], Pat[A]) => Pat[B]): Pat[B] = {
    val b       = Graph.builder
    val itIn    = b.allocToken[A]()
    val itCarry = b.allocToken[B]()
    val inner   = Graph {
      op(itCarry, itIn)
    }
    FoldLeft[A, B](outer = x, z = z, itIn = itIn, itCarry = itCarry, inner = inner)
  }

  def sortWith(lt: (Pat[A], Pat[A]) => Pat[Boolean]): Pat[Pat[A]] = {
    val b     = Graph.builder
    val it    = b.allocToken[(A, A)]()
    val inner = /* Graph */ {
      val (it1, it2) = it.unzip
      lt(it1, it2)
    }
    SortWith(x, it, inner)
  }

  def apply(idx: Pat[Int]): Pat[A] = Apply(x, idx)

  def head: Pat[A] = apply(0)
}

final class PatTuple2Ops[A, B](private val tup: Pat[(A, B)]) extends AnyVal {
  def unzip: (Pat[A], Pat[B]) = {
    (Tuple2_1(tup), Tuple2_2(tup))
  }
}