/*
 *  ListPatterns.scala
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

import de.sciss.patterns.PE.Value

final case class Index[A <: Value](list: PE[A], index: PE.Int, repeats: PE.Int = 1) extends Pattern[A] {
  def toStream(implicit b: StreamGraph.Builder): Stream[A] = ???
}
//
//final case class Pseq(list: PE, repeats: PE = 1, offset: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Pser(list: PE, length: PE = 1, offset: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Shuf(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Rand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class XRand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class WRand(list: PE, weights: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class FSM(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Switch(list: PE, index: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Tuple(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Lace(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Slide(list: PE, repeats: PE = 1, size: PE = 3, step: PE = 1, start: PE = 0, wrap: Boolean = true)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Walk(list: PE, step: PE, dir: PE = 1, start: PE = 0)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}