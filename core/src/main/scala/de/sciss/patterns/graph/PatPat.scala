/*
 *  PatPat.scala
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

import de.sciss.patterns.Types.Top

final case class PatPat[T <: Top](in: Seq[Pat[T]]) extends Pattern[Pat[T]] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Stream[Tx, T#Out[Tx]]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Stream[Tx, T#Out[Tx]]] {
    private[this] val inStreams: Array[Stream[Tx, T#Out[Tx]]] = in.iterator.map(_.expand(ctx, tx0)).toArray
    private[this] val inIdx = ctx.newVar(0)

    def reset()(implicit tx: Tx): Unit = {
      inIdx() = 0
    }

    def hasNext(implicit tx: Tx): Boolean = inIdx() < inStreams.length // && inStreams(inIdx).hasNext

    def next()(implicit tx: Tx):Stream[Tx, T#Out[Tx]] = {
      if (!hasNext) Stream.exhausted()
      val _idx = inIdx()
      val res = inStreams(_idx)
      inIdx() = _idx + 1
//      val res   = s.next()
//      while (!s.hasNext && inIdx < inStreams.length) {
//        inIdx += 1
//        s = inStreams(inIdx)
//      }
      res
    }
  }
}
