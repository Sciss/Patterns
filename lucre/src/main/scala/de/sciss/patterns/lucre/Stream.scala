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
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.patterns
import de.sciss.patterns.lucre.impl.{StreamImpl => Impl}
import de.sciss.patterns.{Graph, Pat}
import de.sciss.serial.DataInput
import de.sciss.synth.proc
import de.sciss.synth.proc.Code.{Example, Import}
import de.sciss.synth.proc.impl.{BasicAuralRunnerImpl, CodeImpl}
import de.sciss.synth.proc.{Runner, Universe}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.Future

object Stream extends Obj.Type with Runner.Factory {
  final val typeId = 300

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

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = ???

  override def init(): Unit = {
    super.init()
    _init
  }

  private lazy val _init: Unit = {
    Code  .init()
    Runner.addFactory(Stream)
  }

  // ---- Code ----

  object Code extends proc.Code.Type {
    final val id = 5

    final val prefix    = "Stream"
    final val humanName = "Stream Graph"

    type Repr = Code

    override def examples: ISeq[Example] = List(
      Example("Brownian", 'b',
        """val b = Brown(0, 127, 2)
          |b
          |""".stripMargin)
    )

    override def defaultSource: String = s"${super.defaultSource}Pat()\n"

    def docBaseSymbol: String = "de.sciss.patterns.graph"

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      import Import._
      proc.Code.registerImports(id, Vec(
        // doesn't work:
        //        "Predef.{any2stringadd => _, _}", // cf. http://stackoverflow.com/questions/7634015/
        Import("de.sciss.numbers.Implicits", All),
        Import("de.sciss.kollflitz.Ops", All),
        Import("de.sciss.lucre.adjunct", Name("Adjunct") :: Nil),
        Import("de.sciss.patterns", All),
        Import("de.sciss.patterns.graph", All),
        Import("de.sciss.patterns.graph.Ops", All)
      ))
      //      proc.Code.registerImports(proc.Code.Action.id, Vec(
      //        "de.sciss.patterns.lucre.Stream"
      //      ))
    }

    // override because we need register imports
    override def init(): Unit = _init

    def mkCode(source: String): Repr = Code(source)
  }
  final case class Code(source: String) extends proc.Code {
    type In     = Unit
    type Out    = Pat[Any]

    def tpe: proc.Code.Type = Code

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
      import scala.reflect.runtime.universe._
      CodeImpl.compileBody[In, Out, Pat[Any], Code](this, typeTag[Pat[Any]])
    }

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      Graph {
        import scala.reflect.runtime.universe._
        CodeImpl.compileThunk[Pat[Any]](this, typeTag[Pat[Any]], execute = true)
      }

    def prelude : String =
      """object Main {
        |  def __result__ : de.sciss.patterns.Pat[_] = {
        |""".stripMargin

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }

  // ---- construction ----

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Stream[S] = Impl()

  // ---- events ----

  sealed trait Update[S <: Sys[S]] {
    def stream: Stream[S]
  }

  final case class PeerChange[S <: Sys[S]](stream: Stream[S]) extends Update[S]
}
trait Stream[S <: Sys[S]] extends Obj[S] with Publisher[S, Stream.Update[S]] {
//  def peer: stm.Var[S#Tx, patterns.Stream[S, _]]

  def peer(implicit tx: S#Tx): patterns.Stream[S, Any]

  def peer_=(value: patterns.Stream[S, _])(implicit tx: S#Tx): Unit
}