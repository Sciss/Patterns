/*
 *  MapItStream.scala
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

final class MapItStream[Tx, A](outer: Pat[Pat[A]], tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, A] {

  private[this] lazy val simpleString = {
    val os0 = outer.toString
    val os  = if (os0.length <= 24) os0 else s"${os0.substring(0, 23)}..."
    s"MapItStream($os)"
  }

  override def toString: String = simpleString

  private[this] val outerStream: Stream[Tx, Pat[A]] = outer.expand(ctx, tx0)

  private[this] val inStream    = ctx.newVar[Stream[Tx, A]](null)
  private[this] val _valid      = ctx.newVar(false)
  private[this] val _hasIn      = ctx.newVar(false)
  private[this] val _hasNext    = ctx.newVar(false)

  def advance()(implicit tx: Tx): Unit = {
    val ohn     = outerStream.hasNext
    _hasNext()  = ohn
    _hasIn()    = false
    logStream(s"$simpleString.advance(): outerStream.hasNext = $ohn")
    if (ohn) {
      val inPat     = outerStream.next()
      val inValue   = inPat.expand
      val ihn       = inValue.hasNext
      logStream(s"$simpleString.advance(): inValue.hasNext = $ihn")
      inStream()    = inValue
      _hasIn()      = true
      _hasNext()    = ihn
    }
  }

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid() = true
      advance()
    }

  def resetOuter()(implicit tx: Tx): Unit = if (_valid()) {
//    logStream(s"$simpleString.resetOuter()")
    _valid() = false
    outerStream.reset()
  }

  def reset()(implicit tx: Tx): Unit = {
    val hi = _hasIn()
    logStream(s"$simpleString.reset(); hasIn = $hi")
    if (hi) {
      val inValue = inStream()
      inValue.reset()
      _hasNext() = inValue.hasNext
    }
  }

  def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasNext()
  }

  def next()(implicit tx: Tx): A = {
    if (!hasNext) Stream.exhausted()
    val in      = inStream()
    val res     = in.next()
    _hasNext()  = in.hasNext
    logStream(s"$simpleString.next() = $res; hasNext = ${in.hasNext}")
    res
  }
}
