/*
 *  StreamImpl.scala
 *  (SoundStreamesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.{event => evt}
import de.sciss.patterns
import de.sciss.patterns.lucre.Stream
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object StreamImpl {
  private final val SER_VERSION = 0x5374  // was "St"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Stream[S] = new New[S]

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
    extends Stream[S] with evt.impl.SingleNode[S, Stream.Update[S]] {
    proc =>

    final def tpe: Obj.Type = Stream

    def peer(implicit tx: S#Tx): patterns.Stream[S, Any] = ???

    def peer_=(value: patterns.Stream[S, _])(implicit tx: S#Tx): Unit = ???

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                     = Targets[Out]
//        val graph     : SynthGraphObj.Var[Out]                  = context(proc.graph)
        connect()
      }

    final def connect()(implicit tx: S#Tx): this.type = {
//      graph.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
//      graph.changed -/-> changed
    }

    object changed extends Changed
      with evt.impl.Generator[S, Stream.Update[S]]
      // with evt.impl.Root[S, Stream.Update[S]]
      // extends evt.impl.EventImpl[S, Stream.Update[S], Stream[S]]
    {

      // final val slot = 4

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Stream.Update[S]] = {
        ???
//        val graphCh     = graph.changed
//        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
//        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Stream.Update[S]]) else None
//
//        val seq0 = graphOpt.fold(Vec.empty[Change[S]]) { u =>
//          Vector(GraphChange(u))
//        }
//        val seq3 = stateOpt.fold(seq0 /* seq2 */) { u =>
//          if (seq0 /* seq2 */.isEmpty) u.changes else seq0 /* seq2 */ ++ u.changes
//        }
//        if (seq3.isEmpty) None else Some(Stream.Update(proc, seq3))
      }
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
    }

    override def toString: String = s"Stream$id"
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = evt.Targets[S](tx0)
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }
  }
}