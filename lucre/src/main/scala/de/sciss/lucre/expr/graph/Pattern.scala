/*
 *  Pattern.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.FromAny
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.{Adjunct, Caching, IChangeEvent, IExpr, IPull, IPush, ITargets, ProductWithAdjuncts, Source, Sys, Txn, Obj => LObj}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction, IControl}
import de.sciss.lucre.impl.{IChangeEventImpl, IChangeGeneratorEvent}
import de.sciss.model.Change
import de.sciss.patterns.lucre.{Context => LContext, Pattern => LPattern}
import de.sciss.patterns.{Stream => PStream}
import de.sciss.serial.{DataInput, TFormat}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object Pattern {
  def apply(): Ex[Pattern] with Obj.Make = Apply()

  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)
    extends ObjCellViewVarImpl[T, LPattern, Pattern](h, key) {

    implicit def format: TFormat[T, Option[LPattern[T]]] =
      TFormat.option

    protected def lower(peer: LPattern[T])(implicit tx: T): Pattern =
      wrap[T](peer)
  }

  implicit object Bridge extends Obj.Bridge[Pattern] with Adjunct.Factory {

    final val id = 1155

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Pattern]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Pattern]] =
      new AbstractCtxCellView[T, Pattern](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Pattern] = value match {
          case st: Pattern  => Some(st)
          case _            => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Pattern] = obj match {
          case peer: LPattern[T]  => Some(wrap(peer))
          case _                  => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Pattern] =
      obj.attr.$[LPattern](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Pattern] = obj match {
      case a: LPattern[T]   => Some(wrap(a))
      case _                => None
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, LPattern[T]], system: Sys): Pattern =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: LPattern[T])(implicit tx: T): Pattern =
    new Impl[T](tx.newHandle(peer), tx.system)

  private final class Impl[T <: Txn[T]](in: Source[T, LPattern[T]], system: Sys)
    extends ObjImplBase[T, LPattern](in, system) with Pattern {

    override type Peer[~ <: Txn[~]] = LPattern[~]
  }

  private[lucre] object Empty extends Pattern {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Pattern] {

    protected def empty: Pattern = Empty

    protected def make()(implicit tx: T): Pattern = {
      val peer = LPattern[T]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply()
    extends Ex[Pattern] with Act with Obj.Make {

    override def productPrefix: String = "Pattern" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Pattern] with IAction[T]

    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

  // ---- operations ----

  implicit final class Ops(private val pat: Ex[Pattern]) extends AnyVal {
    def toStream: ToStream = ToStream(pat)
  }

  private final class ToStreamExpanded[T <: Txn[T]/*, I <: Sys[I]*/](patEx: IExpr[T, Pattern], tx0: T, system: Sys)
                                                                (implicit protected val targets: ITargets[T])
    extends ToStream.Repr[T] with IChangeEventImpl[T, Pattern] with Caching {

    private[this] val ref: Ref[RefVal] = Ref(mkRef(patEx.value(tx0))(tx0))

    patEx.changed.--->(this)(tx0)

    private type CtxI   = LContext[T, tx0.I]
    private type RefVal = Option[(PStream[tx0.I, Any], CtxI)]

    private def mkRef(pat: Pattern)(implicit tx: T): RefVal =
      pat.peer[T].map { lPat =>
        implicit val ctx: CtxI = mkPatCtx(lPat)
        val pPat  = lPat.value  // XXX TODO --- we should observer lPat changes
        val st    = ctx.expandDual(pPat)
        (st, ctx)
      }

    private def mkPatCtx(lPat: LPattern[T])(implicit tx: T): CtxI =
      ??? // LUCRE4 LContext.dual(lPat)(tx)

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Pattern = {
      val inV = pull.expr(patEx)
      if (/*pull.contains(patEx.changed) &&*/ phase.isNow) {
        val refV = mkRef(inV)
        disposeRef(ref.swap(refV))
      }
      inV
    }

    private def disposeRef(refVal: RefVal)(implicit tx: T): Unit =
      refVal.foreach { case (st, _) =>
        st.dispose()(???) // LUCRE4 (tx.inMemoryBridge(tx)) // (system.inMemoryTx(tx))
      }

    def reset()(implicit tx: T): Unit =
      ref().foreach { case (st, _) =>
        st.reset()(???) // LUCRE4 (system.inMemoryTx(tx))
      }

    def hasNext(implicit /*ctx: Context[T],*/ tx: T): Boolean =
      ref().exists { case (st, ctx) =>
        st.hasNext(ctx, ???) // LUCRE4 system.inMemoryTx(tx))
      }

    def next()(implicit /*ctx: Context[T],*/ tx: T): Any = {
      val (st, ctx) = ref().get
      st.next()(ctx, ???) // LUCRE4 system.inMemoryTx(tx))
    }

    def initControl()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = {
      patEx.changed.-/->(this)

      disposeRef(ref.swap(None))
    }
  }

  // XXX TODO DRY with Stream.AbstractNextExpanded
  private abstract class AbstractNextExpanded[T <: Txn[T], A, E](in: ToStream.Repr[T], tx0: T)
                                                                (implicit protected val targets: ITargets[T],
                                                                 from: FromAny[A])
    extends IAction[T] with IExpr[T, E]
      with IChangeGeneratorEvent [T, E]
      with ITriggerConsumer [T, E]
      with Caching {

//    implicit protected final val ctx: patterns.Context[S#I] =
//      patterns.lucre.Context[S#I](tx0.inMemory.system, tx0.inMemory)

    private[this] val ref: Ref[E] = Ref(lower(None)(tx0))

    protected def lower(opt: Option[A])(implicit tx: T): E

    def value(implicit tx: T): E =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: T): Unit = {
      val ch = Change(valueBefore(), trigReceived())
      if (ch.isSignificant) fire(ch)
    }

    protected def trigReceived()(implicit tx: T): E = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: T): E = ref()

    def changed: IChangeEvent[T, E] = this

    private def make()(implicit tx: T): E = {
//      implicit val itx: S#I#Tx = tx.inMemory
      val opt = if (!in.hasNext) None else {
        val any = in.next()
        from.fromAny(any)
      }
      lower(opt)
    }
  }
  
  private final class NextOptionExpanded[T <: Txn[T], A](in: ToStream.Repr[T], tx0: T)
                                                        (implicit targets: ITargets[T], from: FromAny[A])
    extends AbstractNextExpanded[T, A, Option[A]](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: T): Option[A] = opt
  }

  private final class NextExpanded[T <: Txn[T], A](in: ToStream.Repr[T], default: IExpr[T, A], tx0: T)
                                                  (implicit targets: ITargets[T], from: FromAny[A])
    extends AbstractNextExpanded[T, A, A](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: T): A = opt.getOrElse(default.value)
  }

  // XXX TODO DRY with Stream.TakeExpanded
  private final class TakeExpanded[T <: Txn[T], A](in: ToStream.Repr[T], n: IExpr[T, Int]/*, tx0: T*/)
                                                  (implicit protected val targets: ITargets[T],
                                                   from: FromAny[A])
    extends IAction[T] with IExpr[T, Seq[A]]
      with IChangeGeneratorEvent [T, Seq[A]]
      with ITriggerConsumer [T, Seq[A]]
      with Caching {

//    implicit protected val ctx: patterns.Context[S#I] =
//      patterns.lucre.Context[S#I](tx0.inMemory.system, tx0.inMemory)

    private[this] val ref: Ref[Seq[A]] = Ref(Nil)

    def value(implicit tx: T): Seq[A] =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: T): Unit = {
      val ch = Change(valueBefore(), trigReceived())
      if (ch.isSignificant) fire(ch)
    }

    protected def trigReceived()(implicit tx: T): Seq[A] = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: T): Seq[A] = ref()

    def changed: IChangeEvent[T, Seq[A]] = this

    private def make()(implicit tx: T): Seq[A] = {
      val nV  = n .value
      if (nV <= 0) return Nil

      val b   = ISeq.newBuilder[A]
      b.sizeHint(nV)
//      implicit val itx: S#I#Tx = tx.inMemory
      var i = 0
      while (i < nV && in.hasNext) {
        val any = in.next()
        from.fromAny(any) match {
          case Some(a) =>
            b += a
            i += 1
          case None =>
        }
      }
      b.result()
    }
  }

  private final class ResetExpanded[T <: Txn[T]](in: ToStream.Repr[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      in.reset()
  }

  final case class Reset(in: ToStream)
    extends Act {

    override def productPrefix: String = s"Pattern$$Reset" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ResetExpanded[T](in.expand[T])
  }

  final case class NextOption[A](in: ToStream)(implicit from: FromAny[A])
    extends Ex[Option[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$NextOption" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NextOptionExpanded[T, A](in.expand[T], tx)
    }
  }

  final case class Next[A](in: ToStream, default: Ex[A])(implicit from: FromAny[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$Next" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NextExpanded[T, A](in.expand[T], default.expand[T], tx)
    }
  }

  final case class Take[A](in: ToStream, n: Ex[Int])(implicit from: FromAny[A])
    extends Ex[Seq[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$Take" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new TakeExpanded[T, A](in.expand[T], n.expand[T]/*, tx*/)
    }
  }

  object ToStream {
    trait Repr[T <: Txn[T]] extends IControl[T] {
//      private[lucre] def peer(implicit tx: T): PStream[S#I, Any]

      def reset()(implicit tx: T): Unit

      def hasNext(implicit /*ctx: Context[T],*/ tx: T): Boolean
      def next ()(implicit /*ctx: Context[T],*/ tx: T): Any
    }
  }
  final case class ToStream(pat: Ex[Pattern]) extends Control {
    override def productPrefix: String = s"Pattern$$ToStream" // serialization

    type Repr[T <: Txn[T]] = ToStream.Repr[T]

    def reset                           : Act                     = Reset(this)
    def next[A: FromAny]                : Ex[Option [A]] with Act = NextOption(this)
    def next[A: FromAny](default: Ex[A]): Ex[        A]  with Act = Next(this, default)
    def take[A: FromAny](n: Ex[Int])    : Ex[Seq    [A]] with Act = Take(this, n)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ToStreamExpanded[T](pat.expand[T], tx, tx.system)
    }
  }
}
trait Pattern extends Obj {
  type Peer[~ <: Txn[~]] = LPattern[~]
}