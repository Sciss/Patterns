/*
 *  BindImpl.scala
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

package de.sciss.patterns
package graph
package impl

import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object BindImpl extends StreamFactory {
  final val typeId = 0x42696E64 // "Bind"

  def expand[S <: Base[S]](pat: Bind)(implicit ctx: Context[S], tx: S#Tx): Stream[S, Event]  = {
    import pat._
    val id        = tx.newId()
    val mapE: Map[String, Stream[S, Any]] = entries.iterator.map {
        case (key, value) => key -> value.expand[S]
      } .toMap

    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)

    new StreamImpl[S](id = id, mapE = mapE, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
    val id        = tx.readId(in, access)
    val mapE: Map[String, Stream[S, Any]] = Serializer.map[S#Tx, S#Acc, String, Stream[S, Any]].read(in, access)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)

    new StreamImpl[S](id = id, mapE = mapE, _hasNext = _hasNext, valid = valid)
      .asInstanceOf[Stream[S, A]] // XXX TODO --- ugly
  }

  private final class StreamImpl[S <: Base[S]](
    id      : S#Id,
    mapE    : Map[String, Stream[S, Any]],
    _hasNext: S#Var[Boolean],
    valid   : S#Var[Boolean],
  )
    extends Stream[S, Event] {

    protected def typeId: Int = BindImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      Serializer.map[S#Tx, S#Acc, String, Stream[S, Any]].write(mapE, out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      mapE.valuesIterator.foreach(_.dispose())
      _hasNext.dispose()
      valid   .dispose()
    }

    def checkNext()(implicit ctx: Context[S], tx: S#Tx): Boolean = mapE.forall(_._2.hasNext)

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      _hasNext() = checkNext()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      mapE.foreach(_._2.reset())
    }

    private def mkState()(implicit ctx: Context[S], tx: S#Tx): Event = {
      val m = mapE.map {
        case (key, value) =>
          key -> value.next()
      }
      Event(m)
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): Event = {
      if (!hasNext) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}
