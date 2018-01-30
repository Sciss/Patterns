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

  private[this] lazy val simpleString = {
    val os0 = outer.toString
    val os  = if (os0.length <= 24) os0 else s"${os0.substring(0, 23)}..."
    s"MapIterationStream($os)"
  }

  override def toString: String = simpleString

  private[this] val outerStream: Stream[Tx, Stream[Tx, T#Out[Tx]]] = outer.expand(ctx, tx0)

  private[this] val inStream    = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)
  private[this] val _valid      = ctx.newVar(false)
  private[this] val _hasIn      = ctx.newVar(false)
  private[this] val _hasNext    = ctx.newVar(false)

  def advance()(implicit tx: Tx): Unit = {
    val ohn     = outerStream.hasNext
    _hasNext()  = ohn
    _hasIn()    = ohn
    logStream(s"$simpleString.advance(): outerStream.hasNext = $ohn")
    if (ohn) {
      val inValue   = outerStream.next()
      logStream(s"$simpleString.advance(): inValue.hasNext = $ohn")
      inStream()    = inValue
      _hasNext()    = inValue.hasNext
    }
  }

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid() = true
      advance()
    }

  def resetOuter()(implicit tx: Tx): Unit = {
    logStream(s"$simpleString.resetOuter()")
    _valid() = false
  }

  def reset()(implicit tx: Tx): Unit = {
    val hi = _hasIn()
    logStream(s"$simpleString.reset(); hasIn = $hi")
    if (hi) {
      inStream().reset()
    }
  }

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
    logStream(s"$simpleString.next() = $res; hasNext = ${in.hasNext}")
    res
  }
}
