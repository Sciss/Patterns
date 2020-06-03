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

import de.sciss.lucre.adjunct.Adjunct.FromAny
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.event.impl.{IChangeEventImpl, IChangeGenerator}
import de.sciss.lucre.event.{Caching, IChangeEvent, IPull, IPush, ITargets}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction, IControl, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change
import de.sciss.patterns.{Stream => PStream}
import de.sciss.patterns.lucre.{Context => LContext, Pattern => LPattern}
import de.sciss.serial.{DataInput, Serializer}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object Pattern {
  def apply(): Ex[Pattern] with Obj.Make = Apply()

  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends ObjCellViewVarImpl[S, LPattern, Pattern](h, key) {

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[LPattern[S]]] =
      Serializer.option

    protected def lower(peer: LPattern[S])(implicit tx: S#Tx): Pattern =
      wrap[S](peer)
  }

  implicit object Bridge extends Obj.Bridge[Pattern] with Adjunct.Factory {

    final val id = 1155

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Pattern]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Pattern]] =
      new AbstractCtxCellView[S, Pattern](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[Pattern] = value match {
          case st: Pattern  => Some(st)
          case _            => None
        }

        protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[Pattern] = obj match {
          case peer: LPattern[S]  => Some(wrap(peer))
          case _                  => None
        }
      }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[Pattern] =
      obj.attr.$[LPattern](key).map(wrap(_))

    def tryParseObj[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[Pattern] = obj match {
      case a: LPattern[S]   => Some(wrap(a))
      case _                => None
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[S <: Sys[S]](peer: stm.Source[S#Tx, LPattern[S]], system: S): Pattern =
    new Impl[S](peer, system)

  private[lucre] def wrap[S <: Sys[S]](peer: LPattern[S])(implicit tx: S#Tx): Pattern =
    new Impl[S](tx.newHandle(peer), tx.system)

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, LPattern[S]], system: S)
    extends ObjImplBase[S, LPattern](in, system) with Pattern {

    override type Peer[~ <: Sys[~]] = LPattern[~]
  }

  private[lucre] object Empty extends Pattern {
    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
  }

  private final class ApplyExpanded[S <: Sys[S]](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Pattern] {

    protected def empty: Pattern = Empty

    protected def make()(implicit tx: S#Tx): Pattern = {
      val peer = LPattern[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply()
    extends Ex[Pattern] with Act with Obj.Make {

    override def productPrefix: String = "Pattern" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Pattern] with IAction[S]

    def make: Act = this

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S]
    }
  }

  // ---- operations ----

  implicit final class Ops(private val pat: Ex[Pattern]) extends AnyVal {
    def toStream: ToStream = ToStream(pat)
  }

  private final class ToStreamExpanded[S <: Sys[S]/*, I <: Sys[I]*/](patEx: IExpr[S, Pattern], tx0: S#Tx, system: S)
                                                                (implicit protected val targets: ITargets[S])
    extends ToStream.Repr[S] with IChangeEventImpl[S, Pattern] with Caching {

    private[this] val ref: Ref[RefVal] = Ref(mkRef(patEx.value(tx0))(tx0))

    patEx.changed.--->(this)(tx0)

    private type CtxI   = LContext[S, system.I]
    private type RefVal = Option[(PStream[system.I, Any], CtxI)]

    private def mkRef(pat: Pattern)(implicit tx: S#Tx): RefVal =
      pat.peer[S].map { lPat =>
        implicit val ctx: CtxI = mkPatCtx(lPat)
        val pPat  = lPat.value  // XXX TODO --- we should observer lPat changes
        val st    = ctx.expandDual(pPat)
        (st, ctx)
      }

    private def mkPatCtx(lPat: LPattern[S])(implicit tx: S#Tx): CtxI =
      LContext.dual(lPat)(system, tx)

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Pattern = {
      val inV = pull.expr(patEx)
      if (/*pull.contains(patEx.changed) &&*/ phase.isNow) {
        val refV = mkRef(inV)
        disposeRef(ref.swap(refV))
      }
      inV
    }

    private def disposeRef(refVal: RefVal)(implicit tx: S#Tx): Unit =
      refVal.foreach { case (st, _) =>
        st.dispose()(system.inMemoryTx(tx))
      }

    def reset()(implicit tx: S#Tx): Unit =
      ref().foreach { case (st, _) =>
        st.reset()(system.inMemoryTx(tx))
      }

    def hasNext(implicit /*ctx: Context[S],*/ tx: S#Tx): Boolean =
      ref().exists { case (st, ctx) =>
        st.hasNext(ctx, system.inMemoryTx(tx))
      }

    def next()(implicit /*ctx: Context[S],*/ tx: S#Tx): Any = {
      val (st, ctx) = ref().get
      st.next()(ctx, system.inMemoryTx(tx))
    }

    def initControl()(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      patEx.changed.-/->(this)

      disposeRef(ref.swap(None))
    }
  }

  // XXX TODO DRY with Stream.AbstractNextExpanded
  private abstract class AbstractNextExpanded[S <: Sys[S], A, E](in: ToStream.Repr[S], tx0: S#Tx)
                                                                (implicit protected val targets: ITargets[S],
                                                                 from: FromAny[A])
    extends IAction[S] with IExpr[S, E]
      with IChangeGenerator [S, E]
      with ITriggerConsumer [S, E]
      with Caching {

//    implicit protected final val ctx: patterns.Context[S#I] =
//      patterns.lucre.Context[S#I](tx0.inMemory.system, tx0.inMemory)

    private[this] val ref: Ref[E] = Ref(lower(None)(tx0))

    protected def lower(opt: Option[A])(implicit tx: S#Tx): E

    def value(implicit tx: S#Tx): E =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: S#Tx): Unit = {
      val ch = Change(valueBefore(), trigReceived())
      if (ch.isSignificant) fire(ch)
    }

    protected def trigReceived()(implicit tx: S#Tx): E = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: S#Tx): E = ref()

    def changed: IChangeEvent[S, E] = this

    private def make()(implicit tx: S#Tx): E = {
//      implicit val itx: S#I#Tx = tx.inMemory
      val opt = if (!in.hasNext) None else {
        val any = in.next()
        from.fromAny(any)
      }
      lower(opt)
    }
  }
  
  private final class NextOptionExpanded[S <: Sys[S], A](in: ToStream.Repr[S], tx0: S#Tx)
                                                        (implicit targets: ITargets[S], from: FromAny[A])
    extends AbstractNextExpanded[S, A, Option[A]](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: S#Tx): Option[A] = opt
  }

  private final class NextExpanded[S <: Sys[S], A](in: ToStream.Repr[S], default: IExpr[S, A], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], from: FromAny[A])
    extends AbstractNextExpanded[S, A, A](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: S#Tx): A = opt.getOrElse(default.value)
  }

  // XXX TODO DRY with Stream.TakeExpanded
  private final class TakeExpanded[S <: Sys[S], A](in: ToStream.Repr[S], n: IExpr[S, Int]/*, tx0: S#Tx*/)
                                                  (implicit protected val targets: ITargets[S],
                                                   from: FromAny[A])
    extends IAction[S] with IExpr[S, Seq[A]]
      with IChangeGenerator [S, Seq[A]]
      with ITriggerConsumer [S, Seq[A]]
      with Caching {

//    implicit protected val ctx: patterns.Context[S#I] =
//      patterns.lucre.Context[S#I](tx0.inMemory.system, tx0.inMemory)

    private[this] val ref: Ref[Seq[A]] = Ref(Nil)

    def value(implicit tx: S#Tx): Seq[A] =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: S#Tx): Unit = {
      val ch = Change(valueBefore(), trigReceived())
      if (ch.isSignificant) fire(ch)
    }

    protected def trigReceived()(implicit tx: S#Tx): Seq[A] = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: S#Tx): Seq[A] = ref()

    def changed: IChangeEvent[S, Seq[A]] = this

    private def make()(implicit tx: S#Tx): Seq[A] = {
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

  private final class ResetExpanded[S <: Sys[S]](in: ToStream.Repr[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit =
      in.reset()
  }

  final case class Reset(in: ToStream)
    extends Act {

    override def productPrefix: String = s"Pattern$$Reset" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ResetExpanded[S](in.expand[S])
  }

  final case class NextOption[A](in: ToStream)(implicit from: FromAny[A])
    extends Ex[Option[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$NextOption" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextOptionExpanded[S, A](in.expand[S], tx)
    }
  }

  final case class Next[A](in: ToStream, default: Ex[A])(implicit from: FromAny[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$Next" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, A] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextExpanded[S, A](in.expand[S], default.expand[S], tx)
    }
  }

  final case class Take[A](in: ToStream, n: Ex[Int])(implicit from: FromAny[A])
    extends Ex[Seq[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Pattern$$Take" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new TakeExpanded[S, A](in.expand[S], n.expand[S]/*, tx*/)
    }
  }

  object ToStream {
    trait Repr[S <: Sys[S]] extends IControl[S] {
//      private[lucre] def peer(implicit tx: S#Tx): PStream[S#I, Any]

      def reset()(implicit tx: S#Tx): Unit

      def hasNext(implicit /*ctx: Context[S],*/ tx: S#Tx): Boolean
      def next ()(implicit /*ctx: Context[S],*/ tx: S#Tx): Any
    }
  }
  final case class ToStream(pat: Ex[Pattern]) extends Control {
    override def productPrefix: String = s"Pattern$$ToStream" // serialization

    type Repr[S <: Sys[S]] = ToStream.Repr[S]

    def reset                           : Act                     = Reset(this)
    def next[A: FromAny]                : Ex[Option [A]] with Act = NextOption(this)
    def next[A: FromAny](default: Ex[A]): Ex[        A]  with Act = Next(this, default)
    def take[A: FromAny](n: Ex[Int])    : Ex[Seq    [A]] with Act = Take(this, n)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ToStreamExpanded[S](pat.expand[S], tx, tx.system)
    }
  }
}
trait Pattern extends Obj {
  type Peer[~ <: Sys[~]] = LPattern[~]
}