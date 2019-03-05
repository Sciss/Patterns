/*
 *  PatObj.scala
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

import de.sciss.lucre.event.{Dummy, Event, EventLike, Targets}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Copy, Elem, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{expr, stm}
import de.sciss.model.Change
import de.sciss.patterns
import de.sciss.patterns.{Graph, Pat, Stream, stream}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.proc
import de.sciss.synth.proc.Runner
import de.sciss.synth.proc.impl.{BasicRunnerImpl, CodeImpl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

object Pattern extends expr.impl.ExprTypeImpl[Pat[_], Pattern] with Runner.Factory {
  final val typeId = 300

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  def prefix      : String  = "Pattern"
  def humanName   : String  = prefix
  def isSingleton : Boolean = false

  type Repr[~ <: Sys[~]] = Pattern[~]

  def mkRunner[S <: SSys[S]](obj: Pattern[S])(implicit tx: S#Tx, universe: Runner.Universe[S]): Runner[S] =
    BasicRunnerImpl(obj)

  override def init(): Unit = {
    super.init()
    _init
  }

  private lazy val _init: Unit = {
    Code                  .init()
    AuralPatternAttribute .init()
    AuralPatternObj       .init()
    patterns.Obj          .init()

    Runner.addFactory(Pattern)

    Stream.addFactory(stream.AttributeImpl          )
    Stream.addFactory(stream.AudioCueNumChannelsImpl)
    Stream.addFactory(stream.AudioCueNumFramesImpl  )
    Stream.addFactory(stream.AudioCueSampleRateImpl )
    Stream.addFactory(stream.FolderCollectImpl      )
  }

  protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
    extends ConstImpl[S] with Pattern[S]

  private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
    extends VarImpl[S] with Pattern[S]

  // ---- Code ----

//  implicit private object CodeWrapper extends CodeImpl.Wrapper[Unit, Pat[_], Pat[_], Pattern.Code] {
//    def id: Int = Pattern.Code.id
//    def binding = Option.empty[String]
//
//    def wrap(in: Unit)(fun: => Pat[_]): Pat[_] = Graph {
//      fun
//    }
//
//    def blockTag = "de.sciss.patterns.Pat[_]"
//  }

  object Code extends proc.Code.Type {
    final val id    = 5
    final val name  = "Pattern"
    type Repr = Code

    def docBaseSymbol: String = "de.sciss.patterns.graph"

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      proc.Code.registerImports(id, Vec(
        // doesn't work:
        //        "Predef.{any2stringadd => _, _}", // cf. http://stackoverflow.com/questions/7634015/
        "de.sciss.numbers.Implicits._",
        "de.sciss.kollflitz.Ops._",
        "de.sciss.lucre.aux.Aux",
        "de.sciss.patterns._",
        "de.sciss.patterns.graph._",
        "de.sciss.patterns.graph.Ops._"
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
    type Out    = Pat[Any]
    def id: Int = Code.id

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] =
      CodeImpl.compileBody[In, Out, Pat[Any], Code](this)

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      Graph {
        CodeImpl.compileThunk[Pat[Any]](this, execute = true)
      }

    def contextName: String = Code.name

    def docBaseSymbol: String = Code.docBaseSymbol

    def prelude : String =
      """object Main {
        |  def __result__ : de.sciss.patterns.Pat[_] = {
        |""".stripMargin

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }

  // --------------------

  val valueSerializer: ImmutableSerializer[Pat[_]] = Pat.serializer[Any]

  private final val emptyCookie = 4

  override protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)
                                                (implicit tx: S#Tx): _Ex[S] =
    cookie match {
      case `emptyCookie` =>
        val id = tx.readId(in, access)
        new Predefined(id, cookie)
      case _ => super.readCookie(in, access, cookie)
    }

  private val emptyPat = Pat()

  def empty[S <: Sys[S]](implicit tx: S#Tx): _Ex[S] = apply(emptyCookie)

  private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): _Ex[S] = {
    val id = tx.newId()
    new Predefined(id, cookie)
  }

  private final class Predefined[S <: Sys[S]](val id: S#Id, cookie: Int)
    extends Pattern[S] with Expr.Const[S, Pat[_]] {

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    def tpe: stm.Obj.Type = Pattern

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Predefined(txOut.newId(), cookie) // .connect()

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeId)
      out.writeByte(cookie)
      id.write(out)
    }

    def value(implicit tx: S#Tx): Pat[_] = constValue

    def changed: EventLike[S, Change[Pat[_]]] = Dummy[S, Change[Pat[_]]]

    def dispose()(implicit tx: S#Tx): Unit = ()

    def constValue: Pat[_] = cookie match {
      case `emptyCookie` => emptyPat
    }
  }
}
trait Pattern[S <: Sys[S]] extends Expr[S, Pat[_]]