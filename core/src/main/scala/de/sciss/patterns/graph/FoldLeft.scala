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

import de.sciss.lucre.stm.Base

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Pat[A])
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val outerT  = t(outer)
    val zT      = t(z)
    val innerT  = t(inner)
    if (outerT.eq(outer) && zT.eq(z) && innerT.eq(inner)) this else {
      val (itInT    , innerT1) = itIn   .replaceIn(innerT)
      val (itCarryT , innerT2) = itCarry.replaceIn(innerT1)
      copy(outer = outerT, z = zT, itIn = itInT, itCarry = itCarryT, inner = innerT2)
    }
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {

    private[this] val id            = tx0.newId()
    private[this] val outerStream   = outer .expand(ctx, tx0)
    private[this] val _valid        = tx0.newBooleanVar(id, false)
    private[this] val _result       = tx0.newVar[Stream[S, A]](id, null)

    private def validate()(implicit tx: S#Tx): Unit =
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

        val res   = innerRewrite.expand[S]
        _result() = res
      }

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
       _valid() = false
      outerStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      val s = _result()
      s.hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val s = _result()
      val res = s.next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}