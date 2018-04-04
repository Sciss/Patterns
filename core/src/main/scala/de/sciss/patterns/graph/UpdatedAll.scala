/*
 *  UpdatedAll.scala
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

final case class UpdatedAll[A1, A >: A1](in: Pat[A1], idx: Pat[Int], elem: Pat[A]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    val elemT = t(elem)
    if (inT.eq(in) && idxT.eq(idx) && elemT.eq(elem)) this else copy(in = inT, idx = idxT, elem = elemT)
  }

//  private var DEBUG = 0

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, A] {

    private[this] val id          = tx0.newId()
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val idxStream   = idx .expand(ctx, tx0)
    private[this] val elemStream  = elem.expand(ctx, tx0)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val state       = ??? : S#Var[Stream[S, A]] // ctx.newVar[Stream[S, A]](null)(tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      idxStream .reset()
      elemStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit = if (!_valid()) {
      _valid()  = true
//      DEBUG += 1
//      if (DEBUG == 7106) {
//        println("BOO")
//      }
//      if (DEBUG == 7107) {
//        println("AQUI")
//      }
      var vec = inStream.toVector: Vector[A]
      while (idxStream.hasNext && elemStream.hasNext) {
        val idxVal  = idxStream .next()
        val elemVal = elemStream.next()
//        if (idxVal > vec.size) {
//          println("Potzblitz")
//        }
        vec         = vec.updated(idxVal, elemVal)
      }
      val _state  = Stream(vec: _*)
      state()     = _state
    }

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      state().hasNext
    }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      state().next()
    }
  }
}
