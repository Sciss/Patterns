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

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.patterns
import de.sciss.patterns.{Context => PContext, Stream => PStream}
import de.sciss.patterns.lucre.{Context, Stream}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object StreamImpl {
  private final val SER_VERSION = 0x5374  // was "St"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Stream[S] = {
    val s0 = patterns.stream.EmptyImpl[S]()
    new New[S](s0)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Stream[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Stream[S]] {
    def tpe: Obj.Type = Stream
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S] = {
    val targets = Targets.read(in, access)
    new Read(in, access, targets)
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Stream[S] with evt.impl.SingleNode[S, Stream.Update[S]] with stm.Ref[S#Tx, PStream[S, Any]] {
    stream =>

    protected def ref: S#Var[PStream[S, Any]]

    final def tpe: Obj.Type = Stream

    final def peer: stm.Ref[S#Tx, PStream[S, Any]] = this

    def apply()(implicit tx: S#Tx): PStream[S, Any] = ref()

    def update(v: PStream[S, Any])(implicit tx: S#Tx): Unit = {
      val before = ref()
      if (before != v) {
        ref() = v
        changed.fire(Stream.PeerChange(this))
      }
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                   = Targets[Out]
        protected val ref: Out#Var[PStream[Out, Any]] = {
          implicit val ctx: PContext[Out] = Context[Out](txOut.system, txOut)
          implicit val cpy: PStream.Copy[S, Out] = PStream.Copy[S, Out]
          val peerOut = cpy(stream.peer())
          txOut.newVar[PStream[Out, Any]](targets.id, peerOut)
        }
        connect()
      }
    }

    final   def connect   ()(implicit tx: S#Tx): this.type  = this
    private def disconnect()(implicit tx: S#Tx): Unit       = ()

    object changed extends Changed
      with evt.impl.Generator[S, Stream.Update[S]]
      // with evt.impl.Root[S, Stream.Update[S]]
      // extends evt.impl.EventImpl[S, Stream.Update[S], Stream[S]]
    {

      // final val slot = 4

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Stream.Update[S]] =
        Some(pull.resolve)
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      ref.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      ref.dispose()
    }

    override def toString: String = s"Stream$id"
  }

  private final class New[S <: Sys[S]](s0: PStream[S, Any])(implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = evt.Targets[S](tx0)

    protected val ref: S#Var[PStream[S, Any]] = {
      implicit val ctx: PContext[S] = Context[S](tx0.system, tx0)
      tx0.newVar(targets.id, s0)
    }

    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    protected val ref: S#Var[PStream[S, Any]] = {
      // XXX TODO --- we should not need "full" new context,
      // because state is in RNG, and that should not be needed in de-serialization!
      implicit val ctx: PContext[S] = Context[S](tx0.system, tx0)
      tx0.readVar(targets.id, in)
    }
  }
}