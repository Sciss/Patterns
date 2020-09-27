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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.FromAny
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, Caching, IChangeEvent, IExpr, IPush, ITargets, ProductWithAdjuncts, Source, Sys, Txn, Obj => LObj}
import de.sciss.model.Change
import de.sciss.patterns
import de.sciss.patterns.lucre.{Stream => LStream}
import de.sciss.serial.{DataInput, TFormat}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object Stream {
  def apply(): Ex[Stream] with Obj.Make = Apply()

  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)
    extends ObjCellViewVarImpl[T, LStream, Stream](h, key) {

    implicit def format: TFormat[T, Option[LStream[T]]] =
      TFormat.option

    protected def lower(peer: LStream[T])(implicit tx: T): Stream =
      wrap[T](peer)
  }

  implicit object Bridge extends Obj.Bridge[Stream] with Adjunct.Factory {

    final val id = 1154

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Stream]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Stream]] =
      new AbstractCtxCellView[T, Stream](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Stream] = value match {
          case st: Stream => Some(st)
          case _          => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Stream] = obj match {
          case peer: LStream[T] => Some(wrap(peer))
          case _                => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Stream] =
      obj.attr.$[LStream](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Stream] = obj match {
      case a: LStream[T]  => Some(wrap(a))
      case _              => None
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, LStream[T]], system: Sys): Stream =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: LStream[T])(implicit tx: T): Stream =
    new Impl[T](tx.newHandle(peer), tx.system)

  private final class Impl[T <: Txn[T]](in: Source[T, LStream[T]], system: Sys)
    extends ObjImplBase[T, LStream](in, system) with Stream {

    override type Peer[~ <: Txn[~]] = LStream[~]
  }

  private[lucre] object Empty extends Stream {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Stream] {

    protected def empty: Stream = Empty

    protected def make()(implicit tx: T): Stream = {
      val peer = LStream[T]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply()
    extends Ex[Stream] with Act with Obj.Make {

    override def productPrefix: String = "Stream" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Stream] with IAction[T]

    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

  // ---- operations ----

  private abstract class AbstractNextExpanded[T <: Txn[T], A, E](in: IExpr[T, Stream], tx0: T)
                                                                (implicit protected val targets: ITargets[T],
                                                                 from: FromAny[A])
    extends IAction[T] with IExpr[T, E]
    with IChangeGeneratorEvent [T, E]
    with ITriggerConsumer [T, E]
    with Caching {

    // XXX TODO --- this is bloody annoying
    implicit protected final val ctx: patterns.Context[T] = patterns.lucre.Context[T]()(tx0)

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
      val st  = in.value
      val opt = st.peer.flatMap { obj =>
        val p = obj.peer()
        if (!p.hasNext) None else {
          val any = p.next()
          from.fromAny(any)
        }
      }
      lower(opt)
    }
  }

  private final class NextOptionExpanded[T <: Txn[T], A](in: IExpr[T, Stream], tx0: T)
                                                  (implicit targets: ITargets[T], from: FromAny[A])
    extends AbstractNextExpanded[T, A, Option[A]](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: T): Option[A] = opt
  }

  private final class NextExpanded[T <: Txn[T], A](in: IExpr[T, Stream], default: IExpr[T, A], tx0: T)
                                                  (implicit targets: ITargets[T], from: FromAny[A])
    extends AbstractNextExpanded[T, A, A](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: T): A = opt.getOrElse(default.value)
  }

  // XXX TODO --- slightly DRY with `AbstractNextExpanded`
  private final class TakeExpanded[T <: Txn[T], A](in: IExpr[T, Stream], n: IExpr[T, Int], tx0: T)
                                                  (implicit protected val targets: ITargets[T],
                                                   from: FromAny[A])
    extends IAction[T] with IExpr[T, Seq[A]]
      with IChangeGeneratorEvent [T, Seq[A]]
      with ITriggerConsumer [T, Seq[A]]
      with Caching {

    // XXX TODO --- this is bloody annoying
    implicit private[this] val ctx: patterns.Context[T] = patterns.lucre.Context[T]()(tx0)

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
      val st  = in.value
      st.peer.foreach { obj =>
        val p = obj.peer()
        var i = 0
        while (i < nV && p.hasNext) {
          val any = p.next()
          from.fromAny(any) match {
            case Some(a) =>
              b += a
              i += 1
            case None =>
          }
        }
      }
      b.result()
    }
  }

  private final class ResetExpanded[T <: Txn[T]](in: IExpr[T, Stream])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val st = in.value
      st.peer.foreach(_.peer().reset())
    }
  }

  final case class Reset(in: Ex[Stream])
    extends Act {

    override def productPrefix: String = s"Stream$$Reset" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ResetExpanded[T](in.expand[T])
  }

  final case class NextOption[A](in: Ex[Stream])(implicit from: FromAny[A])
    extends Ex[Option[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$NextOption" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NextOptionExpanded[T, A](in.expand[T], tx)
    }
  }

  final case class Next[A](in: Ex[Stream], default: Ex[A])(implicit from: FromAny[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$Next" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, A] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NextExpanded[T, A](in.expand[T], default.expand[T], tx)
    }
  }

  final case class Take[A](in: Ex[Stream], n: Ex[Int])(implicit from: FromAny[A])
    extends Ex[Seq[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$Take" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Seq[A]] with IAction[T]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new TakeExpanded[T, A](in.expand[T], n.expand[T], tx)
    }
  }

  implicit final class Ops(private val st: Ex[Stream]) extends AnyVal {
    def reset                           : Act                     = Reset(st)
    def next[A: FromAny]                : Ex[Option [A]] with Act = NextOption(st)
    def next[A: FromAny](default: Ex[A]): Ex[        A]  with Act = Next(st, default)
    def take[A: FromAny](n: Ex[Int])    : Ex[Seq    [A]] with Act = Take(st, n)
  }
}
trait Stream extends Obj {
  type Peer[~ <: Txn[~]] = LStream[~]
}