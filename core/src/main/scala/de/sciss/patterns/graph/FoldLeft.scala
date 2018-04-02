/*
 *  FoldLeft.scala
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

import de.sciss.patterns.Context.Var

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Pat[A])
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val outerT  = t(outer)
    val zT      = t(z)
    val innerT  = t(inner)
    if (outerT.eq(outer) && zT.eq(z) && innerT.eq(inner)) this else {
      val (itInT    , innerT1) = itIn   .replaceIn(innerT)
      val (itCarryT , innerT2) = itCarry.replaceIn(innerT1)
      copy(outer = outerT, z = zT, itIn = itInT, itCarry = itCarryT, inner = innerT2)
    }
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {

    private[this] val outerStream   = outer .expand(ctx, tx0)

    private[this] val _valid        = ctx.newBooleanVar(false)(tx0)
    private[this] val _result       = ??? : Var[Tx, Stream[Tx, A]] // ctx.newVar[Stream[Tx, A]](null)(tx0)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        logStream("FoldLeft.iterator.validate()")
        val _outer        = outerStream.toVector
        val innerRewrite  = _outer.foldLeft(z) { (y: Pat[A], x: Pat[B]) =>
          val t = new Transform {
            def applyOne[X](in: Pat[X]): Pat[X] = in match {
              case `itIn`     => x.asInstanceOf[Pat[X]]
              case `itCarry`  => y.asInstanceOf[Pat[X]]
              case other      => other
            }
          }
          t(inner)
        }

        val res   = innerRewrite.expand[Tx]
        _result() = res
      }

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
       _valid() = false
      outerStream.reset()
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      val s = _result()
      s.hasNext
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val s = _result()
      val res = s.next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}