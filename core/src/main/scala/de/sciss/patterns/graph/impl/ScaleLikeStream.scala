/*
 *  ScaleLikeStream.scala
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
package impl

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.Widen2

abstract class ScaleLikeStream[S <: Base[S], A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                              outLo: Pat[A2], outHi: Pat[A2], tx0: S#Tx)
                                             (implicit ctx: Context[S], w: Widen2[A1, A2, A]) extends Stream[S, A] {
  private[this] val inStream    = in    .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
  private[this] val inLoStream  = inLo  .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
  private[this] val inHiStream  = inHi  .expand(ctx, tx0).map(w.widen1)(ctx, tx0)
  private[this] val outLoStream = outLo .expand(ctx, tx0).map(w.widen2)(ctx, tx0)
  private[this] val outHiStream = outHi .expand(ctx, tx0).map(w.widen2)(ctx, tx0)

  def dispose()(implicit tx: S#Tx): Unit = ???

  protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A

  final def reset()(implicit tx: S#Tx): Unit = {
    inStream    .reset()
    inLoStream  .reset()
    inHiStream  .reset()
    outLoStream .reset()
    outHiStream .reset()
  }

  final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
    inStream   .hasNext &&
      inLoStream .hasNext &&
      inHiStream .hasNext &&
      outLoStream.hasNext &&
      outHiStream.hasNext

  final def next()(implicit ctx: Context[S], tx: S#Tx): A = {
    if (!hasNext) Stream.exhausted()
    val inVal     = inStream    .next()
    val inLoVal   = inLoStream  .next()
    val inHiVal   = inHiStream  .next()
    val outLoVal  = outLoStream .next()
    val outHiVal  = outHiStream .next()

    calc(inVal, inLoVal, inHiVal, outLoVal, outHiVal)
  }
}