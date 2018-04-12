package de.sciss.patterns
package stream

import de.sciss.lucre.stm.Base

trait ItStream[S <: Base[S], +A] extends Stream[S, A] {
  def resetOuter()(implicit tx: S#Tx): Unit

  def advance()(implicit ctx: Context[S], tx: S#Tx): Unit
}
