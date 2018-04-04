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

import de.sciss.lucre.stm.Base

final case class Format(s: Pat[String], args: Pat[_]*) extends Pattern[String] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, String] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[String] = {
    val sT    = t(s)
    val argsT = args.map(t(_))
    Format(sT, argsT: _*)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, String] {
    private[this] val sStream     = s.expand(ctx, tx0)
    private[this] val argStreams  = args.map(_.expand(ctx, tx0))

    def reset()(implicit tx: S#Tx): Unit = {
      sStream.reset()
      argStreams.foreach(_.reset())
    }

    def hasNext(implicit tx: S#Tx): Boolean = sStream.hasNext && argStreams.forall(_.hasNext)

    def next()(implicit tx: S#Tx): String = {
      val sVal    = sStream.next()
      val argVals = argStreams.map(_.next())
      sVal.format(argVals: _*)
    }
  }
}