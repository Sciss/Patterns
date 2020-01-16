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
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.concurrent.stm.Ref

object Stream {
  def apply[A]()(implicit from: FromAny[A]): Ex[Stream[A]] with Obj.Make = Apply[A]()

  private lazy val _init: Unit =
    Adjunct.addFactory(BridgeFactory)

  def init(): Unit = _init

  private object BridgeFactory extends Adjunct.Factory {
    final val id = 1154

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = {
      val from = Adjunct.readT[FromAny[Any]](in)
      new Bridge[Any]()(from)
    }
  }

  private final class CellViewImpl[S <: Sys[S], A: FromAny](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends ObjCellViewVarImpl[S, LStream, Stream[A]](h, key) {

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[LStream[S]]] =
      Serializer.option

    protected def lower(peer: LStream[S])(implicit tx: S#Tx): Stream[A] =
      wrap[S, A](peer)
  }

  implicit def bridge[A: FromAny]: Obj.Bridge[Stream[A]] = new Bridge()

  private final class Bridge[A]()(implicit from: FromAny[A]) extends Obj.Bridge[Stream[A]] {

    final val id = 1154

    override def write(out: DataOutput): Unit = {
      super.write(out)
      from .write(out)
    }

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S#Tx, Option[Stream[A]]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[S <: Sys[S]](key: String)(implicit tx: S#Tx, context: Context[S]): CellView[S#Tx, Option[Stream[A]]] =
      new AbstractCtxCellView[S, Stream[A]](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: S#Tx): Option[Stream[A]] = value match {
          case st: Stream[_] if st.from == from => Some(st.asInstanceOf[Stream[A]])
          case _                                => None
        }

        protected def tryParseObj(obj: stm.Obj[S])(implicit tx: S#Tx): Option[Stream[A]] = obj match {
          case peer: LStream[S] => Some(wrap(peer))
          case _                => None
        }
      }

    def cellValue[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): Option[Stream[A]] =
      obj.attr.$[LStream](key).map(wrap(_))

    def tryParseObj[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[Stream[A]] = obj match {
      case a: LStream[S]  => Some(wrap(a))
      case _              => None
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[S <: Sys[S], A: FromAny](peer: stm.Source[S#Tx, LStream[S]], system: S): Stream[A] =
    new Impl[S, A](peer, system)

  private[lucre] def wrap[S <: Sys[S], A: FromAny](peer: LStream[S])(implicit tx: S#Tx): Stream[A] =
    new Impl[S, A](tx.newHandle(peer), tx.system)

  private final class Impl[S <: Sys[S], A](in: stm.Source[S#Tx, LStream[S]], system: S)(implicit val from: FromAny[A])
    extends ObjImplBase[S, LStream](in, system) with Stream[A] {

    override type Peer[~ <: Sys[~]] = LStream[~]
  }

  private[lucre] final class Empty[A](implicit val from: FromAny[A])
    extends Stream[A] {

    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
  }

  private final class ApplyExpanded[S <: Sys[S], A: FromAny](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Stream[A]] {

    protected def empty: Stream[A] = new Empty[A]

    protected def make()(implicit tx: S#Tx): Stream[A] = {
      val peer = LStream[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply[A]()(implicit from: FromAny[A])
    extends Ex[Stream[A]] with Act with Obj.Make with ProductWithAdjuncts {

    override def productPrefix: String = "Stream" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Stream[A]] with IAction[S]

    def make: Act = this

    def adjuncts: List[Adjunct] = from :: Nil

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S, A]
    }
  }

  // ---- operations ----

  private final class NextExpanded[S <: Sys[S], A](in: IExpr[S, Stream[A]], tx0: S#Tx)
                                                  (implicit protected val targets: ITargets[S])
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
          st.from.fromAny(any)
        }
      }
    }
  }

  final case class Next[A](in: Ex[Stream[A]]) extends Ex[Option[A]] with Act {
    override def productPrefix: String = s"Stream$$Next" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[A]] with IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NextExpanded[S, A](in.expand[S], tx)
    }
  }

  implicit final class Ops[A](private val st: Ex[Stream[A]]) extends AnyVal {
    def next            : Ex[Option [A]] with Act = Next(st)
    // def take(n: Ex[Int]): Ex[Seq    [A]] with Act = ...
  }
}
trait Stream[A] extends Obj {
  def from: FromAny[A]

  type Peer[~ <: Sys[~]] = LStream[~]
}