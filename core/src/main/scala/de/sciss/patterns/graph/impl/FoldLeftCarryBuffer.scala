/*
 *  FoldLeftCarryBuffer.scala
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

final class FoldLeftCarryBuffer[Tx, T <: Top](tx0: Tx)(implicit ctx: Context[Tx]) {
  type A = T#Out[Tx]

  private[this] val zValueRef   = ctx.newVar(Vector.empty[A])
  private[this] val offsetMap   = ctx.newVar(Map.empty[Stream[Tx, _], Int])
  private[this] val skipped     = ctx.newVar(0)

  def addIt(id: Stream[Tx, _])(implicit tx: Tx): Unit = {
    val om0 = offsetMap()
    if (om0.nonEmpty) ??? // for this to work, we must ensure that `A != Stream[Tx, _]`
    offsetMap() = om0 + (id -> 0)
  }

  def result(implicit tx: Tx): Vector[A] = {
    val res = zValueRef()
    logStream(s"FoldLeftCarryBuffer.result = $res")
    res
  }

  def clear()(implicit tx: Tx): Unit = {
    zValueRef() = Vector.empty
    offsetMap() = offsetMap().map { case (idI, _) => (idI, 0) }
    skipped()   = 0
  }

  def advance(x: Vector[A])(implicit tx: Tx): Unit = {
    logStream(s"FoldLeftCarryBuffer.advance($x)")
    clear()
    zValueRef() = x
  }

  def hasNext(id: Stream[Tx, _])(implicit tx: Tx): Boolean = {
    val om      = offsetMap()
    val skip    = skipped()
    val idx     = om(id) - skip
    val z       = zValueRef()
    idx >= 0 && idx < z.size
  }

  def next(id: Stream[Tx, _])(implicit tx: Tx): A = {
    val om0     = offsetMap()
    val skip0   = skipped()
    val idx0    = om0(id)
    val idx     = idx0 - skip0
    val z       = zValueRef()
    if (idx < 0) throw new IndexOutOfBoundsException(s"id $id, idx0 $idx0, idx $idx")
    if (idx >= z.size) Stream.exhausted()
    val om1     = om0 + (id -> (idx0 + 1))
    offsetMap() = om1
    val res     = z(idx)
    // see if we can forget old values
    val min     = om1.valuesIterator.min
    if (min > 0 && min < z.size) {
      val z1    = z.drop(min)
      zValueRef() = z1
      val skip1   = skip0 + min
      skipped()   = skip1
    }
    res
  }
}
