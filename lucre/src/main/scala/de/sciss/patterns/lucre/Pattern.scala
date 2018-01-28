/*
 *  Pattern.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.patterns.lucre.impl.{PatternImpl => Impl}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.CodeImpl
import de.sciss.{model, patterns}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

object Pattern extends Obj.Type {
  final val typeID = 0x1000E

  // ---- implementation forwards ----

  /** Registers this type and the graph object type.
    * You can use this call to register all Pattern components.
    */
  override def init(): Unit = {
    super   .init()
//    Output  .init()
    GraphObj.init()
    Code    .init()
    AuralPatternAttribute.init()
  }

  def apply[S <: Sys[S]](implicit tx: S#Tx): Pattern[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Pattern[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Pattern[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Pattern[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class GraphChange[S <: Sys[S]](change: model.Change[patterns.Graph[_]]) extends Change[S]

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- Code ----

  object Code extends proc.Code.Type {
    final val id    = 5
    final val name  = "Pattern Graph"
    type Repr = Code

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      proc.Code.registerImports(id, Vec(
        // doesn't work:
        //        "Predef.{any2stringadd => _, _}", // cf. http://stackoverflow.com/questions/7634015/
        "de.sciss.patterns._",
        "de.sciss.patterns.Types._",
        "de.sciss.patterns.graph._"
      ))
//      proc.Code.registerImports(proc.Code.Action.id, Vec(
//        "de.sciss.patterns.lucre.Pattern"
//      ))
    }

    // override because we need register imports
    override def init(): Unit = _init

    def mkCode(source: String): Repr = Code(source)
  }
  final case class Code(source: String) extends proc.Code {
    type In     = Unit
    type Out    = patterns.Graph[_]
    def id: Int = Code.id

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
      import Impl.CodeWrapper
      CodeImpl.compileBody[In, Out, Code](this)
    }

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out = {
      import Impl.CodeWrapper
      CodeImpl.execute[In, Out, Code](this, in)
    }

    def contextName: String = Code.name

    def updateSource(newText: String): Code = copy(source = newText)
  }
}

/** The `Pattern` trait is the basic entity representing a sound process. */
trait Pattern[S <: Sys[S]] extends Obj[S] with Publisher[S, Pattern.Update[S]] {
  /** The variable synth graph function of the process. */
  def graph: GraphObj.Var[S]

//  def outputs: Pattern.Outputs[S]
}