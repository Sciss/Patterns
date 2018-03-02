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

import de.sciss.patterns.Types.{Num, NumBool, NumFrac, Ord, ToNum, Widen}
import de.sciss.patterns.graph.{UnaryOp => UnOp, BinaryOp => BinOp, _}

import scala.language.higherKinds

final class PatOps[A](private val x: Pat[A]) extends AnyVal {
  def take(length: Pat[Int] = 1): Pat[A] = Take(x, length)
  def drop(length: Pat[Int]    ): Pat[A] = Drop(x, length)

//  def head: Pat[A] = take(1)
  def tail: Pat[A] = drop(1)

  def splitAt(index: Pat[Int]): (Pat[A], Pat[A]) = (take(index), drop(index))

  /** Updates a single element. */
  def updated[B >: A](index: Pat[Int], elem: B): Pat[B] = Updated(x, index, elem)

  /** Zips the indices with the elements, and then replaces these pairs.
    * Eagerly expands the entire input pattern.
    *
    * Be careful that this hang if both `index` and `elem` are constants!
    */
  def updatedAll[B >: A](index: Pat[Int], elem: Pat[B]): Pat[B] = UpdatedAll(x, index, elem)

  // unary across sequence

  def differentiate(implicit num: Num[A]): Pat[A] = Differentiate(x)

  def sum(implicit num: Num[A]): Pat[A] = Sum(x)

  // unary element-wise

  def unary_-         (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Neg[A](), x)
  def unary_!         (implicit num: NumBool[A] ): Pat[A]         = UnOp(UnOp.Not[A](), x)
  def abs             (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Abs[A](), x)

  def toInt     [C[_]](implicit num: ToNum[C, A]): Pat[C[Int]]    = UnOp(UnOp.ToInt   [C, A](), x)
  def toDouble  [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.ToDouble[C, A](), x)

  def ceil            (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Ceil    [A](), x)
  def floor           (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Floor   [A](), x)
  def frac            (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Frac    [A](), x)
  def squared         (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Squared [A](), x)
  def cubed           (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Cubed   [A](), x)

  def sqrt      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Sqrt[C, A](), x)
  def exp       [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Exp [C, A](), x)

  def reciprocal      (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Reciprocal[A](), x)

  def midicps   [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Midicps   [C, A](), x)
  def cpsmidi   [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Cpsmidi   [C, A](), x)
  def midiratio [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Midiratio [C, A](), x)
  def ratiomidi [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Ratiomidi [C, A](), x)
  def dbamp     [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Dbamp     [C, A](), x)
  def ampdb     [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Ampdb     [C, A](), x)

  def octcps    [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Octcps    [C, A](), x)
  def cpsoct    [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Cpsoct    [C, A](), x)
  def log       [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Log       [C, A](), x)
  def log2      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Log2      [C, A](), x)
  def log10     [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Log10     [C, A](), x)
  def sin       [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Sin       [C, A](), x)
  def cos       [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Cos       [C, A](), x)
  def tan       [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Tan       [C, A](), x)
  def asin      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Asin      [C, A](), x)
  def acos      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Acos      [C, A](), x)
  def atan      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Atan      [C, A](), x)
  def sinh      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Sinh      [C, A](), x)
  def cosh      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Cosh      [C, A](), x)
  def tanh      [C[_]](implicit num: ToNum[C, A]): Pat[C[Double]] = UnOp(UnOp.Tanh      [C, A](), x)

  def rand            (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Rand  [A](), x)
  def rand2           (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Rand2 [A](), x)

  // binary

  def ++ [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2]): Pat[A2] =
    Cat(x, that)

  def :+ [A1, A2](elem: A1)(implicit w: Widen[A, A1, A2]): Pat[A2] =
    x ++ Pat(elem)

  def +: [A1, A2](elem: A1)(implicit w: Widen[A1, A, A2]): Pat[A2] =
    Pat(elem) ++ x

  def + [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.Plus[A2]()
    BinOp(op, x, that)
  }

  def - [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.Minus[A2]()
    BinOp(op, x, that)
  }

  def * [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.Times[A2]()
    BinOp(op, x, that)
  }

  def / [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: NumFrac[A2]): Pat[A2] = {
    val op = BinOp.Div[A2]()
    BinOp(op, x, that)
  }

  def % [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.%[A2]()
    BinOp(op, x, that)
  }

  def mod[A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.Mod[A2]()
    BinOp(op, x, that)
  }

  def roundTo[A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], num: Num[A2]): Pat[A2] = {
    val op = BinOp.RoundTo[A2]()
    BinOp(op, x, that)
  }

  def <  [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinOp.Lt[A2]()
    BinOp(op, x, that)
  }

  def <= [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinOp.Leq[A2]()
    BinOp(op, x, that)
  }

  def >  [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinOp.Gt[A2]()
    BinOp(op, x, that)
  }

  def >= [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2], ord: Ord[A2]): Pat[Boolean] = {
    val op = BinOp.Geq[A2]()
    BinOp(op, x, that)
  }

  def sig_== [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2]): Pat[Boolean] = {
    val op = BinOp.Eq[A2]()
    BinOp(op, x, that)
  }

  def sig_!= [A1, A2](that: Pat[A1])(implicit w: Widen[A, A1, A2]): Pat[Boolean] = {
    val op = BinOp.Neq[A2]()
    BinOp(op, x, that)
  }

  def linlin[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit w: Widen[A, A1, A2], num: NumFrac[A2]): Pat[A2] =
    LinLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def stutter(n: Pat[Int]): Pat[A] = Stutter(x, n)

  def distinct: Pat[A] = Distinct(x)

  /** Same as `length`. */
  def size    : Pat[Int]  = Length(x)
  def length  : Pat[Int]  = Length(x)

  def indices : Pat[Int]  = Indices(x)

  def sorted(implicit ord: Ord[A]): Pat[A] = Sorted(x)

  def shuffle: Pat[A] = Shuffle(x)
  def choose : Pat[A] = Choose (x)

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

  // ---- requiring nested type A ----

// because we need `ev`, this damages Scala type inference on the use site

//  /** Similar to a monadic `map` but with the constraint
//    * the element type must be a (nested) pattern.
//    */
//  def map[B, C](f: Pat[C] => Pat[B])(implicit ev: A <:< Pat[C]): Pat[Pat[B]] = {
//    val xc    = x.asInstanceOf[Pat[Pat[C]]]
//    val b     = Graph.builder
//    val it    = b.allocToken[C]()
//    //    val level = b.level + 1
//    val inner = Graph {
//      f(it)
//    }
//    PatMap[C, B](outer = xc, it = it, inner = inner /* , innerLevel = level */)
//  }
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