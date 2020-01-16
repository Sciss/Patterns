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
import de.sciss.lucre.expr.impl.ITriggerConsumer
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change
import de.sciss.patterns
import de.sciss.patterns.lucre.{Stream => LStream}
import de.sciss.serial.{DataInput, Serializer}

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

  private final class NextExpanded[S <: Sys[S], A](in: IExpr[S, Stream], tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S], from: FromAny[A])
    extends IAction[S] with IExpr[S, Option[A]]
      with IChangeGenerator [S, Option[A]]
      with ITriggerConsumer [S, Option[A]]
      with Caching {

    // XXX TODO --- this is bloody annoying
    implicit private val ctx: patterns.Context[S] = patterns.lucre.Context[S](tx0.system, tx0)

    private[this] val ref = Ref[Option[A]](None)

    def value(implicit tx: S#Tx): Option[A] =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: S#Tx): Unit = {
      val ch = Change(valueBefore(), trigReceived())
      if (ch.isSignificant) fire(ch)
    }

    protected def trigReceived()(implicit tx: S#Tx): Option[A] = {
      val now = make()
      ref() = now
      now
    }

    protected def valueBefore()(implicit tx: S#Tx): Option[A] = ref()

    def changed: IChangeEvent[S, Option[A]] = this

    private def make()(implicit tx: S#Tx): Option[A] = {
      val st = in.value
      st.peer.flatMap { obj =>
        val p = obj.peer()
        if (!p.hasNext) None else {
          val any = p.next()
          from.fromAny(any)
        }
      }
    }
  }

  final case class Next[A](in: Ex[Stream])(implicit from: FromAny[A])
    extends Ex[Option[A]] with Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Stream$$Next" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]] with IAction[S]

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextExpanded[S, A](in.expand[S], tx)
    }
  }

  implicit final class Ops(private val st: Ex[Stream]) extends AnyVal {
    def next[A: FromAny] : Ex[Option [A]] with Act = Next(st)
    // def take(n: Ex[Int]): Ex[Seq    [A]] with Act = ...
  }
}
trait Stream extends Obj {
  type Peer[~ <: Sys[~]] = LStream[~]
}