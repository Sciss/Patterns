/*
 *  Patterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

/** aka Pseries */
final case class ArithmSeq[A](start: PE[A], step: PE[A], length: PE[Int] = Int.MaxValue)(implicit num: Numeric[A])
  extends Pattern[A] {

  def toStream(implicit b: StreamGraph.Builder): Stream[A] =
    new stream.ArithmSeq(start = start.expand, step = step.expand, length = length.expand)
}