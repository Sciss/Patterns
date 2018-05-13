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

import de.sciss.patterns.Types.{Eq, Num, NumBool, NumDouble, NumFrac, NumInt, Ord, ScalarOrd, ToNum, Widen, WidenToDouble, Widen2}
import de.sciss.patterns.graph.{BinaryOp => BinOp, UnaryOp => UnOp, _}

final class PatOps[A](private val x: Pat[A]) extends AnyVal {
  /** Takes only the `length` first elements of the input pattern,
    * or less if the input pattern is shorter.
    *
    * E.g. `Pat(4, 5, 6).take(2) == Pat(4, 5)`
    */
  def take(length: Pat[Int] = 1): Pat[A] = Take(x, length)

  /** Drops the first `length` elements of the input pattern.
    * If the length is greater than the input pattern length, the
    * result will be empty.
    *
    * E.g. `Pat(4, 5, 6).drop(2) == Pat(6)`
    */
  def drop(length: Pat[Int]    ): Pat[A] = Drop(x, length)

//  def head: Pat[A] = take(1)

  /** Drops the first element of the pattern.
    *
    * E.g. `Pat(4, 5, 6).tail == Pat(5, 6)`.
    */
  def tail: Pat[A] = drop(1)

  /** Shorthand for calling both `take` and `drop`. */
  def splitAt(index: Pat[Int]): (Pat[A], Pat[A]) = (take(index), drop(index))

  /** Updates a single element. In other words, only one element from
    * `index` and `elem` is ever read.
    *
    * E.g. `Pat(4, 5, 6).updated(2, 7) == Pat(4, 5, 7)`
    */
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

  def unary_-   (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Neg[A](), x)
  def unary_!   (implicit num: NumBool[A] ): Pat[A]         = UnOp(UnOp.Not[A](), x)
  def unary_~   (implicit num: NumInt[A]  ): Pat[A]         = UnOp(UnOp.BitNot[A](), x)
  def abs       (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Abs[A](), x)

  def toDouble  (implicit to: ToNum[A]): Pat[to.Double]     = UnOp(UnOp.ToDouble[A, to.Double]()(to), x)
  def toInt     (implicit to: ToNum[A]): Pat[to.Int]        = UnOp(UnOp.ToInt   [A, to.Int   ]()(to), x)

  def ceil      (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Ceil    [A](), x)
  def floor     (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Floor   [A](), x)
  def frac      (implicit num: NumFrac[A] ): Pat[A]         = UnOp(UnOp.Frac    [A](), x)
  def signum    (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Signum  [A](), x)
  def squared   (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Squared [A](), x)
  def cubed     (implicit num: Num[A]     ): Pat[A]         = UnOp(UnOp.Cubed   [A](), x)

  def sqrt   [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Sqrt[A, B](), x)
  def exp    [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Exp [A, B](), x)

  def reciprocal[B](implicit w: Widen[A, B], num: NumFrac[B]): Pat[B] = UnOp(UnOp.Reciprocal[A, B](), x)

  def midiCps  [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Midicps   [A, B](), x)
  def cpsMidi  [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Cpsmidi   [A, B](), x)
  def midiRatio[B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Midiratio [A, B](), x)
  def ratioMidi[B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Ratiomidi [A, B](), x)
  def dbAmp    [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Dbamp     [A, B](), x)
  def ampDb    [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Ampdb     [A, B](), x)

  def octCps   [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Octcps    [A, B](), x)
  def cpsOct   [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Cpsoct    [A, B](), x)
  def log      [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Log       [A, B](), x)
  def log2     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Log2      [A, B](), x)
  def log10    [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Log10     [A, B](), x)
  def sin      [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Sin       [A, B](), x)
  def cos      [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Cos       [A, B](), x)
  def tan      [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Tan       [A, B](), x)
  def asin     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Asin      [A, B](), x)
  def acos     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Acos      [A, B](), x)
  def atan     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Atan      [A, B](), x)
  def sinh     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Sinh      [A, B](), x)
  def cosh     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Cosh      [A, B](), x)
  def tanh     [B](implicit wd: WidenToDouble[A, B]): Pat[B] = UnOp(UnOp.Tanh      [A, B](), x)

  def rand      (implicit num: Num[A]       ): Pat[A]           = UnOp(UnOp.Rand  [A](), x)
  def rand2     (implicit num: Num[A]       ): Pat[A]           = UnOp(UnOp.Rand2 [A](), x)
  def coin      (implicit num: NumDouble[A] ): Pat[num.Boolean] = UnOp(UnOp.Coin  [A, num.Boolean]()(num), x)

  // binary

  def ++ [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2]): Pat[A2] = Cat(x, that)
  def :+ [A1, A2](elem: A1)     (implicit w: Widen2[A, A1, A2]): Pat[A2] = x ++ Pat(elem)
  def +: [A1, A2](elem: A1)     (implicit w: Widen2[A1, A, A2]): Pat[A2] = Pat(elem) ++ x

  def +         [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Plus     [A2](), x, that)
  def -         [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Minus    [A2](), x, that)
  def *         [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Times    [A2](), x, that)
  def /         [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: NumFrac   [A2]): Pat[A2] = BinOp(BinOp.Div      [A2](), x, that)
  def %         [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.ModJ        [A2](), x, that)
  def mod       [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Mod      [A2](), x, that)

  def sig_== (that: Pat[A])(implicit eq: Eq[A]): Pat[eq.Boolean] = BinOp(BinOp.Eq [A, eq.Boolean]()(eq), x, that)
  def sig_!= (that: Pat[A])(implicit eq: Eq[A]): Pat[eq.Boolean] = BinOp(BinOp.Neq[A, eq.Boolean]()(eq), x, that)

  def <  (that: Pat[A])(implicit ord: Ord[A]): Pat[ord.Boolean] = BinOp(BinOp.Lt [A, ord.Boolean]()(ord), x, that)
  def >  (that: Pat[A])(implicit ord: Ord[A]): Pat[ord.Boolean] = BinOp(BinOp.Gt [A, ord.Boolean]()(ord), x, that)
  def <= (that: Pat[A])(implicit ord: Ord[A]): Pat[ord.Boolean] = BinOp(BinOp.Leq[A, ord.Boolean]()(ord), x, that)
  def >= (that: Pat[A])(implicit ord: Ord[A]): Pat[ord.Boolean] = BinOp(BinOp.Geq[A, ord.Boolean]()(ord), x, that)

  def min       [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Min      [A2](), x, that)
  def max       [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Max      [A2](), x, that)

  def &   (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.BitAnd[A](), x, that)
  def |   (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.BitOr [A](), x, that)
  def ^   (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.BitXor[A](), x, that)

  def lcm (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.Lcm   [A](), x, that)
  def gcd (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.Gcd   [A](), x, that)

  def roundTo   [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.RoundTo  [A2](), x, that)
  def roundUpTo [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.RoundUpTo[A2](), x, that)
  def trunc     [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num       [A2]): Pat[A2] = BinOp(BinOp.Trunc    [A2](), x, that)

  def atan2     [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Pat[A2] = BinOp(BinOp.Atan2    [A2](), x, that)
  def hypot     [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Pat[A2] = BinOp(BinOp.Hypot    [A2](), x, that)
  def hypotApx  [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Pat[A2] = BinOp(BinOp.Hypotx   [A2](), x, that)
  def pow       [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: NumDouble [A2]): Pat[A2] = BinOp(BinOp.Pow      [A2](), x, that)

  def <<  (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.LeftShift         [A](), x, that)
  def >>  (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.RightShift        [A](), x, that)
  def >>> (that: Pat[A])(implicit num: NumInt[A]): Pat[A] = BinOp(BinOp.UnsignedRightShift[A](), x, that)

  def difSqr[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Difsqr[A2](), x, that)
  def sumSqr[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Sumsqr[A2](), x, that)
  def sqrSum[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Sqrsum[A2](), x, that)
  def sqrDif[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Sqrdif[A2](), x, that)
  def absDif[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Absdif[A2](), x, that)

  def clip2 [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Clip2 [A2](), x, that)
  def excess[A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Excess[A2](), x, that)
  def fold2 [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Fold2 [A2](), x, that)
  def wrap2 [A1, A2](that: Pat[A1])(implicit w: Widen2[A, A1, A2], num: Num[A2]): Pat[A2] = BinOp(BinOp.Wrap2 [A2](), x, that)

  def linLin[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumFrac[A2]): Pat[A2] =
    LinLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def linExp[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Pat[A2] =
    LinExp[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def expLin[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Pat[A2] =
    ExpLin[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def expExp[A1, A2](inLo: Pat[A], inHi: Pat[A], outLo: Pat[A1], outHi: Pat[A1])
                    (implicit w: Widen2[A, A1, A2], num: NumDouble[A2]): Pat[A2] =
    ExpExp[A, A1, A2](x, inLo = inLo, inHi = inHi, outLo = outLo, outHi = outHi)

  def stutter(n: Pat[Int]): Pat[A] = Stutter(x, n)

  def distinct: Pat[A] = Distinct(x)

  /** Same as `length`. */
  def size    : Pat[Int]  = Length(x)

  /** Yields the number of elements in the input pattern.
    *
    * E.g. `Pat(4, 5, 6).length == Pat(3)`
    */
  def length  : Pat[Int]  = Length(x)

  def indices : Pat[Int]  = Indices(x)

  def sorted(implicit ord: ScalarOrd[A]): Pat[A] = Sorted(x)

  /** Randomly changes the positions of the elements in the input pattern.
    * __Warning:__ the input must be finite.
    */
  def shuffle: Pat[A] = Shuffle(x)

  /** Chooses a random single element from the input pattern. */
  def choose: Pat[A] = Choose(x)

  /** Wraps each element in a singleton pattern. For example,
    * `Pat(1, 2, 3)` becomes `Pat(Pat(1), Pat(2), Pat(3))`.
    */
  def bubble: Pat[Pat[A]] = Bubble(x)

  def grouped(size: Pat[Int]): Pat[Pat[A]] = Grouped(x, size)

//  def bubbleFilter(f: Pat[A] => Pat[Boolean]): Pat[A] =
//    bubble.filter(f).flatten

  /** Finds first index where this pattern
    * contains a given other pattern as a slice.
    *
    *  @param  that    the sequence to test
    *  @return  the first index such that the elements of this pattern starting at this index
    *           match the elements of pattern `that`, or `-1` of no such subsequence exists.
    */
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
//    val level = b.level + 1
    val inner = Graph {
      f(it)
    }
    FlatMap(outer = x, it = it, inner = inner /*, innerLevel = level */)
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

final class SeqToPatOps[A](private val seq: Seq[A]) extends AnyVal {
  def toPat: Pat[A] = Pat(seq: _*)
}