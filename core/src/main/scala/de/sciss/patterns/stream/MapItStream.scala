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
package stream

import de.sciss.lucre.stm.Base
import de.sciss.serial.DataOutput

final class MapItStream[S <: Base[S], A](outer: Pat[Pat[A]], tx0: S#Tx)(implicit ctx: Context[S])
  extends Stream[S, A] {

  private[this] val id          = tx0.newId()
  private[this] val outerStream = outer.expand(ctx, tx0)
  private[this] val inStream    = tx0.newVar[Stream[S, A]](id, null)
  private[this] val _valid      = tx0.newBooleanVar(id, false)
  private[this] val _hasIn      = tx0.newBooleanVar(id, false)
  private[this] val _hasNext    = tx0.newBooleanVar(id, false)

  private[this] lazy val simpleString = {
    val os0 = outer.toString
    val os  = if (os0.length <= 24) os0 else s"${os0.substring(0, 23)}..."
    s"MapItStream($os)"
  }

  protected def typeId: Int = ???

  protected def writeData(out: DataOutput): Unit = ???

  def dispose()(implicit tx: S#Tx): Unit = ???

  override def toString: String = simpleString

  def advance()(implicit tx: S#Tx): Unit = {
    _valid()    = true // require(_valid())
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

  private def validate()(implicit tx: S#Tx): Unit =
    if (!_valid()) {
//      _valid() = true
      advance()
    }

  def resetOuter()(implicit tx: S#Tx): Unit = if (_valid.swap(false)) {
//    logStream(s"$simpleString.resetOuter()")
    outerStream.reset()
  }

  def reset()(implicit tx: S#Tx): Unit = {
    val hi = _hasIn()
    logStream(s"$simpleString.reset(); hasIn = $hi")
    if (hi) {
      val inValue   = inStream()
      inValue.reset()
      val ihn     = inValue.hasNext
      _hasNext()  = ihn
    }
  }

  def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
    validate()
    _hasNext()
  }

  def next()(implicit ctx: Context[S], tx: S#Tx): A = {
    if (!hasNext) Stream.exhausted()
    val in      = inStream()
    val res     = in.next()
    _hasNext()  = in.hasNext
    logStream(s"$simpleString.next() = $res; hasNext = ${in.hasNext}")
    res
  }
}
