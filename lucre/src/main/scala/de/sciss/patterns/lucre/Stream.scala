/*
 *  Stream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.{expr, stm}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.patterns
import de.sciss.patterns.Pat
import de.sciss.patterns.lucre.impl.{StreamImpl => Impl}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.BasicAuralRunnerImpl
import de.sciss.synth.proc.{Runner, Universe}

object Stream extends Obj.Type with Runner.Factory {
  final val typeId = 301

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  def prefix      : String  = "Stream"
  def humanName   : String  = prefix
  def isSingleton : Boolean = false

  type Repr[~ <: Sys[~]] = Stream[~]

  def tpe: Obj.Type = this

  def tryParse(value: Any): Option[Pat[_]] = value match {
    case x: Pat[_]  => Some(x)
    case _          => None
  }

  def mkRunner[S <: SSys[S]](obj: Stream[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    BasicAuralRunnerImpl(obj)

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  override def init(): Unit = {
    super.init()
    _init
  }

  private lazy val _init: Unit = {
//    Code  .init()
    AuralStreamAttribute.init()
    AuralStreamObj      .init()
    Runner.addFactory(Stream)
    expr.graph.Stream.init()
  }

  // ---- construction ----

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Stream[S] = Impl()

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Stream[S]] = Impl.serializer[S]

  // ---- events ----

  sealed trait Update[S <: Sys[S]] {
    def stream: Stream[S]
  }

  final case class PeerChange[S <: Sys[S]](stream: Stream[S]) extends Update[S]
}
trait Stream[S <: Sys[S]] extends Obj[S] with Publisher[S, Stream.Update[S]] {
  def peer: stm.Ref[S#Tx, patterns.Stream[S, Any]]

  implicit def context: patterns.Context[S]
}