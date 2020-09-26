/*
 *  AttributeImpl.scala
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

package de.sciss.patterns.stream

import de.sciss.lucre.{Adjunct, Exec, Ident, Var}
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.patterns.{Context, Obj, Stream, graph}
import de.sciss.serial.{DataInput, DataOutput}

object AttributeImpl extends StreamFactory {
  final val typeId = 0x61747472 // "attr"

  def expand[T <: Exec[T], A](pat: graph.Attribute[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat.{ex, key}
    val id        = tx.newId()
    val _next     = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)
    new StreamImpl[T, A](id = id, key = key, _next = _next, _hasNext = _hasNext, valid = valid)(ex)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val key       = in.readUTF()
    val _next     = PatElem.readVar[T, Any](id, in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)
    val ex        = Adjunct.readT[Obj.Adjunct[Any]](in)

    new StreamImpl[T, Any](id = id, key = key, _next = _next, _hasNext = _hasNext, valid = valid)(ex)
  }

  private final class StreamImpl[T <: Exec[T], A](id      : Ident[T],
                                                  key     : String,
                                                  _next   : Var[T, A],
                                                  _hasNext: Var[T, Boolean],
                                                  valid   : Var[T, Boolean]
                                                 )(
                                                   implicit ex: Obj.Adjunct[A]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val nextOut     = PatElem.copyVar[Out, A](idOut, _next())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())
      new StreamImpl[Out, A](id = idOut, key = key, _next = nextOut, _hasNext = hasNextOut, valid = validOut)(ex)
    }

    protected def typeId: Int = AttributeImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      out.writeUTF(key)
      _next   .write(out)
      _hasNext.write(out)
      valid   .write(out)
      ex     .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      _next   .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: T): Unit =
      valid() = false

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      val v: LContext.Attribute.Value[A] = ctx.requestInput(LContext.Attribute[A](key))
      v.peer match {
        case Some(value) =>
          _next()     = value
          _hasNext()  = true

        case None =>
          _hasNext()  = false
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res     = _next()
      _hasNext()  = false
      res
    }
  }
}
