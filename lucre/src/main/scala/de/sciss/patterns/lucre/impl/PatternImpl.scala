/*
 *  PatternImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object PatternImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Pattern[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Pattern[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Pattern[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Pattern[S]] {
    def tpe: Obj.Type = Pattern
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val targets = Targets.read(in, access)
    val serVer  = in.readShort()
    if (serVer == SER_VERSION) {
      new Read(in, access, targets)
    } else {
      sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }
  }

  private final val SER_VERSION = 0x5061  // "Pa"

  private sealed trait Impl[S <: Sys[S]]
    extends Pattern[S] with evt.impl.SingleNode[S, Pattern.Update[S]] {
    proc =>

    // --- impl ----

    final def tpe: Obj.Type = Pattern

    override def toString: String = s"Pattern$id"

    // --- rendering ---

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out] = Targets[Out]
        val graph: GraphObj.Var[Out]        = context(proc.graph)
        connect()
      }

    // ---- key maps ----

    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    final def connect()(implicit tx: S#Tx): this.type = {
      graph.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      graph.changed -/-> changed
    }

    object changed extends Changed
      with evt.impl.Generator[S, Pattern.Update[S]] {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Pattern.Update[S]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Pattern.Update[S]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Pattern.Change[S]]) { u =>
          Vector(Pattern.GraphChange(u))
        }

        val seq3 = stateOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Pattern.Update(proc, seq3))
      }
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      graph.dispose()
    }
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = Targets(tx0)
    val graph: GraphObj.Var[S] = GraphObj.newVar(GraphObj.empty)
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    val graph: GraphObj.Var[S] = GraphObj.readVar(in, access)
  }
}