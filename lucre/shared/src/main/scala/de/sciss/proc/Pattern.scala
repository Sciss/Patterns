/*
 *  Pattern.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.expr.ExElem
import de.sciss.lucre.impl.{DummyEvent, ExprTypeImpl}
import de.sciss.lucre.{Copy, Elem, Event, EventLike, Expr, Ident, Txn, expr, synth, Obj => LObj, Var => LVar}
import de.sciss.model.Change
import de.sciss.patterns.graph.Pat
import de.sciss.patterns.lucre.{AuralPatternAttribute, AuralPatternObj, Stream}
import de.sciss.patterns.{Graph, stream, Stream => PStream}
import de.sciss.{patterns, proc}
import de.sciss.proc.Code.{Example, Import}
import de.sciss.proc.impl.{BasicAuralRunnerImpl, CodeImpl}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.Future

object Pattern extends ExprTypeImpl[Pat[_], Pattern] with Runner.Factory {
  final val typeId = 300

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  def prefix      : String  = "Pattern"
  def humanName   : String  = prefix
  def isSingleton : Boolean = false

  type Repr[~ <: Txn[~]] = Pattern[~]

  def apply[T <: Txn[T]]()(implicit tx: T): Var[T] = newVar[T](empty)

  def tryParse(value: Any): Option[Pat[_]] = value match {
    case x: Pat[_]  => Some(x)
    case _          => None
  }

  def mkRunner[T <: synth.Txn[T]](obj: Pattern[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
    BasicAuralRunnerImpl(obj)

  // initializes the entire library
  override def init(): Unit = {
    super.init()
    _init
  }

  private lazy val _init: Unit = {
    Code                  .init()
    AuralPatternAttribute .init()
    AuralPatternObj       .init()
    patterns.Obj          .init()
    Stream                .init()

    Runner.addFactory(Pattern)
    expr.graph.Pattern.init()

    PStream.addFactory(stream.AttributeImpl          )
    PStream.addFactory(stream.AudioCueNumChannelsImpl)
    PStream.addFactory(stream.AudioCueNumFramesImpl  )
    PStream.addFactory(stream.AudioCueSampleRateImpl )
    PStream.addFactory(stream.FolderCollectImpl      )

    ExElem.addProductReaderSq({
      import expr.graph.{Pattern => _Pattern, Stream => _Stream}
      Seq(
        _Pattern, _Pattern.Reset, _Pattern.NextOption, _Pattern.Next, _Pattern.Take, _Pattern.ToStream,
        _Stream, _Stream.Reset, _Stream.NextOption, _Stream.Next, _Stream.Take,
      )
    })
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Pattern[T]

  private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
    extends VarImpl[T] with Pattern[T]

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
    final val id = 5

    final val prefix    = "Pattern"
    final val humanName = "Pattern Graph"

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
        Import("de.sciss.lucre", Name("Adjunct") :: Nil),
        Import("de.sciss.patterns", List(Name("Event"), Name("Scale"), Name("Tuning"))),
        Import("de.sciss.patterns.graph", All),
        Import("de.sciss.patterns.lucre.PatImport", All)
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

    def tpe: proc.Code.Type = Code

    private val resCl = classOf[Pat[Any]]

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
      CodeImpl.compileBody[In, Out, Pat[Any], Code](this, resCl)
    }

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      Graph {
        CodeImpl.compileThunk[Pat[Any]](this, resCl, execute = true)
      }

    def prelude : String =
      s"""object Main {
         |  def __result__ : ${resCl.getName}[_] = {
         |""".stripMargin

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }

  // --------------------

  val valueFormat: ConstFormat[Pat[_]] = Pat.format[Any]

  private final val emptyCookie = 4

  override protected def readCookie[T <: Txn[T]](in: DataInput, cookie: Byte)
                                                (implicit tx: T): E[T] =
    cookie match {
      case `emptyCookie` =>
        val id = tx.readId(in)
        new Predefined(id, cookie)
      case _ =>
        super.readCookie(in, cookie)
    }

  private val emptyPat = Pat()

  def empty[T <: Txn[T]](implicit tx: T): E[T] = apply(emptyCookie)

  private def apply[T <: Txn[T]](cookie: Int)(implicit tx: T): E[T] = {
    val id = tx.newId()
    new Predefined(id, cookie)
  }

  private final class Predefined[T <: Txn[T]](val id: Ident[T], cookie: Int)
    extends Pattern[T] with Expr.Const[T, Pat[_]] {

    def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

    def tpe: LObj.Type = Pattern

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Predefined(txOut.newId(), cookie) // .connect()

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeId)
      out.writeByte(cookie)
      id.write(out)
    }

    def value(implicit tx: T): Pat[_] = constValue

    def changed: EventLike[T, Change[Pat[_]]] = DummyEvent()

    def dispose()(implicit tx: T): Unit = ()

    def constValue: Pat[_] = cookie match {
      case `emptyCookie` => emptyPat
    }
  }
}
trait Pattern[T <: Txn[T]] extends Expr[T, Pat[_]]