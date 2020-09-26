/*
 *  StreamImpl.scala
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

package de.sciss.patterns.lucre.impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{GeneratorEvent, ObjFormat, SingleEventNode}
import de.sciss.lucre.synth.AnyTxn
import de.sciss.lucre.{Copy, Elem, Obj, Pull, Ref, Txn, Var}
import de.sciss.patterns
import de.sciss.patterns.lucre.{Context, Stream}
import de.sciss.patterns.{Stream => PStream}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object StreamImpl {
  private final val SER_VERSION = 0x5374  // "St"

  def apply[T <: Txn[T]]()(implicit tx: T): Stream[T] = {
    val s0 = patterns.stream.EmptyImpl[T]()
    new New[T](s0, tx)
  }

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Stream[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Stream[T]] = anyFmt.asInstanceOf[Fmt[T]]

  private val anyFmt = new Fmt[AnyTxn]

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Stream[T]] {
    def tpe: Obj.Type = Stream
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Stream[T] = {
    val targets = Targets.read(in)
    new Read(in, targets, tx)
  }

  private sealed trait Impl[T <: Txn[T]]
    extends Stream[T] with SingleEventNode[T, Stream.Update[T]] with Ref[T, PStream[T, Any]] {
    stream =>

    // ---- abstract ----

    implicit override def context: Context.Persistent[T]

    protected def ref: Var[T, PStream[T, Any]]

    // ---- impl ----

    final def tpe: Obj.Type = Stream

    final def peer: Ref[T, PStream[T, Any]] = this

    def apply()(implicit tx: T): PStream[T, Any] = ref()

    def update(v: PStream[T, Any])(implicit tx: T): Unit = {
      val before = ref()
      if (before != v) {
        ref() = v
        changed.fire(Stream.PeerChange(this))
      }
    }

    def swap(value: PStream[T, Any])(implicit tx: T): PStream[T, Any] = {
      val before = apply()
      update(value)
      before
    }

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      new Impl[Out] { out =>
        protected val targets: Targets[Out] = Targets[Out]()
        implicit val context: Context.Persistent[Out] = stream.context.copy[Out]()
        protected val ref: Var[Out, PStream[Out, Any]] = {
          implicit val cpy: PStream.Copy[T, Out] = PStream.Copy[T, Out]
          val peerOut = cpy(stream.peer())
          targets.id.newVar[PStream[Out, Any]](peerOut)
        }
        connect()
      }
    }

    final   def connect   ()(implicit tx: T): this.type  = this
    private def disconnect()(implicit tx: T): Unit       = ()

    object changed extends Changed
      with GeneratorEvent[T, Stream.Update[T]]
      // with evt.impl.Root[T, Stream.Update[T]]
      // extends evt.impl.EventImpl[T, Stream.Update[T], Stream[T]]
    {

      // final val slot = 4

      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Stream.Update[T]] =
        Some(pull.resolve)
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      context .write(out)
      ref     .write(out)
    }

    final protected def disposeData()(implicit tx: T): Unit = {
      disconnect()
      context.dispose()
      ref.dispose()
    }

    override def toString: String = s"Stream$id"
  }

  private final class New[T <: Txn[T]](s0: PStream[T, Any], tx0: T) extends Impl[T] {
    protected val targets: Targets[T] = Targets[T]()(tx0)

    implicit val context: Context.Persistent[T] = Context.persistent(id)(tx0)

    protected val ref: Var[T, PStream[T, Any]] = {
      implicit val tx: T = tx0
      targets.id.newVar(s0)
    }

    connect()(tx0)
  }

  private final class Read[T <: Txn[T]](in: DataInput, protected val targets: Targets[T], tx0: T)
    extends Impl[T] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    implicit val context: Context.Persistent[T] = Context.readPersistent(in)(tx0)

    protected val ref: Var[T, PStream[T, Any]] =
      targets.id.readVar(in)
  }
}