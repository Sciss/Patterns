/*
 *  BindImpl.scala
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

package de.sciss.patterns
package stream

import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.patterns.graph.Bind
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writer}

object BindImpl extends StreamFactory {
  final val typeId = 0x42696E64 // "Bind"

  def expand[T <: Exec[T]](pat: Bind)(implicit ctx: Context[T], tx: T): Stream[T, Event]  = {
    import pat._
    val id        = tx.newId()
    val mapE: Map[String, Stream[T, Any]] = entries.iterator.map {
        case (key, value) => key -> value.expand[T]
      } .toMap

    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T](id = id, mapE = mapE, _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val mapE: Map[String, Stream[T, Any]] = TFormat.map[T, String, Stream[T, Any]].readT(in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)

    new StreamImpl[T](id = id, mapE = mapE, _hasNext = _hasNext, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T]](
                                                id      : Ident[T],
                                                mapE    : Map[String, Stream[T, Any]],
                                                _hasNext: Var[T, Boolean],
                                                valid   : Var[T, Boolean]
  )
    extends Stream[T, Event] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, Event] = {
      val idOut        = txOut.newId()
      val mapEOut: Map[String, Stream[Out, Any]] = mapE.map {
        case (key, value) => key -> c(value)
      }

      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out](id = idOut, mapE = mapEOut, _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = BindImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      Writer.map[String, Stream[T, Any]].write(mapE, out)
      _hasNext.write(out)
      valid   .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      mapE.valuesIterator.foreach(_.dispose())
      _hasNext.dispose()
      valid   .dispose()
    }

    def checkNext()(implicit ctx: Context[T], tx: T): Boolean = mapE.forall(_._2.hasNext)

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      _hasNext() = checkNext()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      mapE.foreach(_._2.reset())
    }

    private def mkState()(implicit ctx: Context[T], tx: T): Event = {
      val m = mapE.map {
        case (key, value) =>
          key -> value.next()
      }
      Event(m)
    }

    def next()(implicit ctx: Context[T], tx: T): Event = {
      if (!hasNext) Stream.exhausted()
      val res = mkState()
      _hasNext() = checkNext()
      res
    }
  }
}
