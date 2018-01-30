/*
 *  MapIterationStream.scala
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

import de.sciss.patterns.Types.Top

final class MapIterationStream[Tx, T <: Top](outer: Pat[Pat[T]], tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, T#Out[Tx]] {

  private[this] val outerStream: Stream[Tx, Stream[Tx, T#Out[Tx]]] = outer.expand(ctx, tx0)

  private[this] val inStream    = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)
  private[this] val _valid      = ctx.newVar(false)
  private[this] val _hasNext    = ctx.newVar(false)

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid()    = true
      val ohn     = outerStream.hasNext
      _hasNext()  = ohn
      if (ohn) {
        val inValue   = outerStream.next()
        inStream()    = inValue
        _hasNext()    = inValue.hasNext
      }
    }

  def reset()(implicit tx: Tx): Unit =
    _valid() = false

  def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasNext()
  }

  def next()(implicit tx: Tx): T#Out[Tx] = {
    validate()
    if (!_hasNext()) Stream.exhausted()
    val in      = inStream()
    val res     = in.next()
    _hasNext()  = in.hasNext
    res
  }
}
