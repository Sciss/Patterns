/*
 *  BinaryOp.scala
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
package graph

import de.sciss.patterns.Types.{Bridge, Num, Top}

object BinaryOp {
  sealed abstract class Op[T <: Top] {
    def apply(a: T#Out, b: T#Out): T#Out
  }

  final case class Plus[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply(a: T#Out, b: T#Out): T#Out = num.plus(a, b)
  }

  final case class Times[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply(a: T#Out, b: T#Out): T#Out = num.times(a, b)
  }

  final case class RoundTo[T <: Top]()(implicit num: Num[T]) extends Op[T] {
    def apply(a: T#Out, b: T#Out): T#Out = num.times(a, b)
  }
}
final case class BinaryOp[T1 <: Top, T2 <: Top, T <: Top](op: BinaryOp.Op[T], a: Pat[T1], b: Pat[T2])
                                                         (implicit br: Bridge[T1, T2, T])
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Stream[T#Out] = {
    val ai = a.expand.map(br.lift1)
    val bi = b.expand.map(br.lift2)
    (ai zip bi).map { case (av, bv) => op(av, bv) }
  }
}
