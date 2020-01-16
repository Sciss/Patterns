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

import de.sciss.lucre.adjunct.Adjunct.FromAny
import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{Caching, IChangeEvent, IPush, ITargets}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change
import de.sciss.patterns
import de.sciss.patterns.lucre.{Stream => LStream}
import de.sciss.serial.{DataInput, Serializer}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object Stream {
  def apply(): Ex[Stream] with Obj.Make = Apply()

  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends ObjCellViewVarImpl[S, LStream, Stream](h, key) {

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[LStream[S]]] =
      Serializer.option

    protected def lower(peer: LStream[S])(implicit tx: S#Tx): Stream =
      wrap[S](peer)
  }

  implicit object Bridge extends Obj.Bridge[Stream] with Adjunct.Factory {

    final val id = 1154

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Stream]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Stream]] =
      new AbstractCtxCellView[S, Stream](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[Stream] = value match {
          case st: Stream => Some(st)
          case _          => None
        }

        protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[Stream] = obj match {
          case peer: LStream[S] => Some(wrap(peer))
          case _                => None
        }
      }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[Stream] =
      obj.attr.$[LStream](key).map(wrap(_))

    def tryParseObj[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[Stream] = obj match {
      case a: LStream[S]  => Some(wrap(a))
      case _              => None
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[S <: Sys[S]](peer: stm.Source[S#Tx, LStream[S]], system: S): Stream =
    new Impl[S](peer, system)

  private[lucre] def wrap[S <: Sys[S]](peer: LStream[S])(implicit tx: S#Tx): Stream =
    new Impl[S](tx.newHandle(peer), tx.system)

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, LStream[S]], system: S)
    extends ObjImplBase[S, LStream](in, system) with Stream {

    override type Peer[~ <: Sys[~]] = LStream[~]
  }

  private[lucre] object Empty extends Stream {
    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
  }

  private final class ApplyExpanded[S <: Sys[S]](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Stream] {

    protected def empty: Stream = Empty

    protected def make()(implicit tx: S#Tx): Stream = {
      val peer = LStream[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply()
    extends Ex[Stream] with Act with Obj.Make {

    override def productPrefix: String = "Stream" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Stream] with IAction[S]

    def make: Act = this

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S]
    }
  }

  // ---- operations ----

  private abstract class AbstractNextExpanded[S <: Sys[S], A, E](in: IExpr[S, Stream], tx0: S#Tx)
                                                                (implicit protected val targets: ITargets[S],
                                                                 from: FromAny[A])
    extends IAction[S] with IExpr[S, E]
    with IChangeGenerator [S, E]
    with ITriggerConsumer [S, E]
    with Caching {

    // XXX TODO --- this is bloody annoying
    implicit protected final val ctx: patterns.Context[S] = patterns.lucre.Context[S](tx0.system, tx0)

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

  private final class NextOptionExpanded[S <: Sys[S], A](in: IExpr[S, Stream], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], from: FromAny[A])
    extends AbstractNextExpanded[S, A, Option[A]](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: S#Tx): Option[A] = opt
  }

  private final class NextExpanded[S <: Sys[S], A](in: IExpr[S, Stream], default: IExpr[S, A], tx0: S#Tx)
                                                  (implicit targets: ITargets[S], from: FromAny[A])
    extends AbstractNextExpanded[S, A, A](in, tx0) {

    protected def lower(opt: Option[A])(implicit tx: S#Tx): A = opt.getOrElse(default.value)
  }

  // XXX TODO --- slightly DRY with `AbstractNextExpanded`
  private final class TakeExpanded[S <: Sys[S], A](in: IExpr[S, Stream], n: IExpr[S, Int], tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S],
                                                   from: FromAny[A])
    extends IAction[S] with IExpr[S, Seq[A]]
      with IChangeGenerator [S, Seq[A]]
      with ITriggerConsumer [S, Seq[A]]
      with Caching {

    // XXX TODO --- this is bloody annoying
    implicit private[this] val ctx: patterns.Context[S] = patterns.lucre.Context[S](tx0.system, tx0)

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
      val st  = in.value
      val nV  = n .value
      if (nV <= 0) return Nil

      val b   = ISeq.newBuilder[A]
      b.sizeHint(nV)
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

  private final class ResetExpanded[S <: Sys[S]](in: IExpr[S, Stream])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val st = in.value
      st.peer.foreach(_.peer().reset())
    }
  }

  final case class Reset(in: Ex[Stream])
    extends Act {

    override def productPrefix: String = s"Stream$$Reset" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ResetExpanded[S](in.expand[S])
  }

  final case class NextOption[A](in: Ex[Stream])(implicit from: FromAny[A])
    extends Ex[Option[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$NextOption" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextOptionExpanded[S, A](in.expand[S], tx)
    }
  }

  final case class Next[A](in: Ex[Stream], default: Ex[A])(implicit from: FromAny[A])
    extends Ex[A] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$Next" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, A] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextExpanded[S, A](in.expand[S], default.expand[S], tx)
    }
  }

  final case class Take[A](in: Ex[Stream], n: Ex[Int])(implicit from: FromAny[A])
    extends Ex[Seq[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$Take" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Seq[A]] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new TakeExpanded[S, A](in.expand[S], n.expand[S], tx)
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
  type Peer[~ <: Sys[~]] = LStream[~]
}