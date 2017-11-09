/*
 *  Truncate.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.patterns.Types.{IntTop, Top}

trait Truncate[T <: Top] extends Pattern[T] {
  // ---- abstract ----

  protected val in: Pat[T]
  protected def length: Pat[IntTop]

  protected def truncate(it: Iterator[T#Out], n: Int): Iterator[T#Out]

  // ---- impl ----

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val lenIt = length.expand
    if (lenIt.isEmpty) Iterator.empty
    else {
      val lenVal  = lenIt.next()
      val inIt    = in.expand
      truncate(inIt, lenVal)
    }
  }
}
