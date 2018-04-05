/*
 *  Sorted.scala
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
import de.sciss.patterns.Types.{Aux, ScalarOrd}
import de.sciss.serial.DataOutput

final case class Sorted[A](in: Pat[A])(implicit ord: ScalarOrd[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id          = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = tx0.newBooleanVar(id, false)
    private[this] val sortedIt  = tx0.newVar[Stream[S, A]](id, null)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        val xs      = inStream.toList
        sortedIt()  = Stream(xs.sortWith(ord.lt): _*)
      }

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      sortedIt().hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      validate()
      sortedIt().next()
    }
  }
}
