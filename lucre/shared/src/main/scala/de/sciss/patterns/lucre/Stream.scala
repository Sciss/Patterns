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

import de.sciss.lucre.{Obj, Publisher, Ref, Txn, expr, synth}
import de.sciss.patterns
import de.sciss.patterns.graph.Pat
import de.sciss.patterns.lucre.impl.{StreamImpl => Impl}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.synth.proc.impl.BasicAuralRunnerImpl
import de.sciss.synth.proc.{Runner, Universe}

object Stream extends Obj.Type with Runner.Factory {
  final val typeId = 301

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  def prefix      : String  = "Stream"
  def humanName   : String  = prefix
  def isSingleton : Boolean = false

  type Repr[~ <: Txn[~]] = Stream[~]

  def tpe: Obj.Type = this

  def tryParse(value: Any): Option[Pat[_]] = value match {
    case x: Pat[_]  => Some(x)
    case _          => None
  }

  def mkRunner[T <: synth.Txn[T]](obj: Stream[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
    BasicAuralRunnerImpl(obj)

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

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

  def apply[T <: Txn[T]]()(implicit tx: T): Stream[T] = Impl()

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Stream[T] = Impl.read(in)

  implicit def serializer[T <: Txn[T]]: TFormat[T, Stream[T]] = Impl.format[T]

  // ---- events ----

  sealed trait Update[T <: Txn[T]] {
    def stream: Stream[T]
  }

  final case class PeerChange[T <: Txn[T]](stream: Stream[T]) extends Update[T]
}
trait Stream[T <: Txn[T]] extends Obj[T] with Publisher[T, Stream.Update[T]] {
  def peer: Ref[T, patterns.Stream[T, Any]]

  implicit def context: patterns.Context[T]
}