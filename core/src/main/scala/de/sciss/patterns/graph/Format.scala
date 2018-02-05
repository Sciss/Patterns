/*
 *  Format.scala
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

import de.sciss.patterns.Types.StringTop

final case class Format(s: Pat.String, args: Pat[_]*) extends Pattern[StringTop] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, String] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, String] {
    private[this] val sStream     = s.expand(ctx, tx0)
    private[this] val argStreams  = args.map(_.expand(ctx, tx0))

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = sStream.hasNext && argStreams.forall(_.hasNext)

    def next()(implicit tx: Tx): String = {
      val sVal    = sStream.next()
      val argVals = argStreams.map(_.next())
      sVal.format(argVals: _*)
    }
  }
}