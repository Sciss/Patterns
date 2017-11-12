/*
 *  Ppar.scala
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

import de.sciss.patterns.Types.Top

final case class Ppar[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Iterator[T#Out] = ???
}
