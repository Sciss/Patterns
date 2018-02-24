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

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Pat[A])
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[A] = {
    val outerT  = t(outer)
    val zT      = t(z)
    val innerT  = t(inner)
    if (outerT.eq(outer) && zT.eq(z) && innerT.eq(inner)) this else copy(outer = outerT, z = zT, inner = innerT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {

    private[this] val outerStream   = outer .expand(ctx, tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _result       = ctx.newVar[Stream[Tx, A]](null)

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

    def reset()(implicit tx: Tx): Unit = {
      println("TODO: FoldLeft.reset")
      logStream("FoldLeft.iterator.reset()")
      _valid() = false
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