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

import scala.collection.immutable.{IndexedSeq => Vec}

/** aka Pseries */
final case class ArithmSeq(start: PE = 0, step: PE = 1, length: PE = Long.MaxValue) extends Pattern.SingleOut {
  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???

  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
}