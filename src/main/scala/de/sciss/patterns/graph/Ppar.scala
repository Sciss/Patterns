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

final case class Ppar(list: Seq[Pat.Event], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[Event] {

  type Out = Event#Out

  def iterator(implicit ctx: Context): Iterator[Out] = ???
}