/*
 *  SortWith.scala
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

import de.sciss.patterns.Types.{BooleanTop, Top, Tuple2Top}

final case class SortWith[T <: Top](outer: Pat[Pat[T]], it: It[Tuple2Top[T, T]], inner: Graph[BooleanTop])
  extends Pattern[Pat[T]] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Stream[Tx, T#Out[Tx]]] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, Stream[Tx, T#Out[Tx]]] {

    private[this] val outerStream = outer.expand(ctx, tx0)
    private[this] val innerStream = inner.expand(ctx, tx0)
    private[this] val _valid      = ctx.newVar(false)

    private[this] val sortedIt    = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val xs      = outerStream.toList
        sortedIt()  = ???
      }

    private def perform()(implicit tx: Tx): Unit = {
      ???
//      val vec = outerStream.toVector
//      val len = vec.length
//      val b = newBuilder
//      if (len == 1) b ++= this
//      else if (len > 1) {
//        b.sizeHint(len)
//        val arr = new Array[AnyRef](len)  // Previously used ArraySeq for more compact but slower code
//        var i = 0
//        for (x <- this) {
//          arr(i) = x.asInstanceOf[AnyRef]
//          i += 1
//        }
//        java.util.Arrays.sort(arr, ord.asInstanceOf[Ordering[Object]])
//        i = 0
//        while (i < arr.length) {
//          b += arr(i).asInstanceOf[A]
//          i += 1
//        }
//      }
//      b.result()
    }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      sortedIt().hasNext
    }

    def next()(implicit tx: Tx): Stream[Tx, T#Out[Tx]] = {
      validate()
      sortedIt().next()
      ???
    }
  }
}