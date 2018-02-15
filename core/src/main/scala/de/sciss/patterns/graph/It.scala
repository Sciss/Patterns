/*
 *  It.scala
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

/** A glue element to make `map` and `flatMap` work. */
final case class It[A](token: Int) extends Pattern[A] { pat =>
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream(s"$pat.iterator")
    new StreamImpl[Tx](tx)
  }

//  private final class StreamImplOLD[Tx](implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
//    private[this] val _outer = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)
//
//    private[this] val _valid = ctx.newVar(false)
//
//    private def validate()(implicit tx: Tx): Unit =
//      if (!_valid()) {
//        _valid() = true
//        _outer() = ctx.mkOuterStream(token)
//      }
//
//    def reset()(implicit tx: Tx): Unit =
//      _valid() = false
//
//    def hasNext(implicit tx: Tx): Boolean = {
//      validate()
//      val res = _outer().hasNext
//      logStream(s"$pat.iterator.hasNext = $res")
//      // if (token == 1) {
//      //   new Exception().fillInStackTrace().printStackTrace()
//      // }
//      res
//    }
//
//    def next ()(implicit tx: Tx): T#Out[Tx]  = {
//      validate()
//      val res = _outer().next()
//      logStream(s"$pat.iterator.next() = $res")
//      res
//    }
//  }

  def transform(t: Transform): Pat[A] = this

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val refStream = ctx.mkOuterStream(token)(tx0)

    def reset()(implicit tx: Tx): Unit      = refStream.reset()
    def hasNext(implicit tx: Tx): Boolean   = refStream.hasNext
    def next ()(implicit tx: Tx): A         = refStream.next()
  }
}
