/*
 *  Pattern.scala
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

import de.sciss.patterns.PE.Value

import scala.collection.immutable.{IndexedSeq => Vec}

object Pattern {
//  trait ZeroOut   extends Pattern[Unit, Unit]
//  trait SingleOut extends SomeOut[Any /* StreamOut */]
//  trait MultiOut  extends SomeOut[Vec[Any /* StreamOut */]]
//
//  protected sealed trait SomeOut[S] extends Pattern[StreamInLike, S] with PE.Lazy
//
//  def unwrap[S](source: Pattern[S], args: Vec[StreamInLike])(implicit b: StreamGraph.Builder): StreamInLike = {
//    var uIns    = Vector.empty: Vec[StreamIn]
//    var uInsOk  = true
//    var exp     = 0
//    args.foreach(_.unbubble match {
//      case u: StreamIn => if (uInsOk) uIns :+= u
//      case g: StreamInGroup =>
//        exp     = math.max(exp, g.numOutputs)
//        uInsOk  = false // don't bother adding further UGenIns to uIns
//    })
//    if (uInsOk) {
//      // aka uIns.size == args.size
//      source.makeStream(uIns)
//    } else {
//      // rewrap(args, exp)
//      StreamInGroup(Vector.tabulate(exp)(i => unwrap(source, args.map(_.unwrap(i)))))
//    }
//  }
//
//  def unwrap(source: Pattern.ZeroOut, args: Vec[StreamInLike])(implicit b: StreamGraph.Builder): Unit = {
//    var uIns    = Vector.empty: Vec[StreamIn]
//    var uInsOk  = true
//    var exp     = 0
//    args.foreach(_.unbubble match {
//      case u: StreamIn => if (uInsOk) uIns :+= u
//      case g: StreamInGroup =>
//        exp     = math.max(exp, g.numOutputs)
//        uInsOk  = false // don't bother adding further UGenIns to uIns
//    })
//    if (uInsOk) {
//      // aka uIns.size == args.size
//      source.makeStream(uIns)
//    } else {
//      // rewrap(args, exp)
//      var i = 0
//      while (i < exp) {
//        unwrap(source, args.map(_.unwrap(i)))
//        i += 1
//      }
//    }
//  }

//  /** Simple forwarder to `in.expand` that can be used to access
//    * the otherwise package-private method.
//    */
//  def expand(in: PE)(implicit b: StreamGraph.Builder): StreamInLike = in.expand
//
//  /** Simple forwarder to `in.outputs` that can be used to access
//    * the otherwise package-private method.
//    */
//  def outputs(in: StreamInLike): Vec[StreamInLike] = in.outputs
//
//  /** Simple forwarder to `in.flatOutputs` that can be used to access
//    * the otherwise package-private method.
//    */
//  def flatOutputs(in: StreamInLike): Vec[StreamIn] = in.flatOutputs
//
//  /** Simple forwarder to `in.unbubble` that can be used to access
//    * the otherwise package-private method.
//    */
//  def unbubble(in: StreamInLike): StreamInLike = in.unbubble
//
//  /** Simple forwarder to `in.unwrap` that can be used to access
//    * the otherwise package-private method.
//    */
//  def unwrapAt(in: StreamInLike, index: Int): StreamInLike = in.unwrap(index)
}

/* sealed */ trait Pattern[+A <: Value] /* [U, S] */ extends PE.Lazy[A] {
  final def name: String = productPrefix

  // private[patterns] def makeStream(args: Vec[StreamIn])/* (implicit b: stream.Builder) */: S
}