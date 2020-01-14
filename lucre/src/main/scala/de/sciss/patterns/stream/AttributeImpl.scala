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

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.patterns.{Context, Obj, Stream, graph}
import de.sciss.serial.{DataInput, DataOutput}

object AttributeImpl extends StreamFactory {
  final val typeId = 0x61747472 // "attr"

  def expand[S <: Base[S], A](pat: graph.Attribute[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat.{key, ex}
    val id        = tx.newId()
    val _next     = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
    new StreamImpl[S, A](id = id, key = key, _next = _next, _hasNext = _hasNext, valid = valid)(ex)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val key       = in.readUTF()
    val _next     = PatElem.readVar[S, Any](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val ex        = Adjunct.readT[Obj.Adjunct[Any]](in)

    new StreamImpl[S, Any](id = id, key = key, _next = _next, _hasNext = _hasNext, valid = valid)(ex)
  }

  private final class StreamImpl[S <: Base[S], A](id      : S#Id,
                                                  key     : String,
                                                  _next   : S#Var[A],
                                                  _hasNext: S#Var[Boolean],
                                                  valid   : S#Var[Boolean]
                                                 )(
                                                   implicit ex: Obj.Adjunct[A]
  )
    extends Stream[S, A] {

    protected def typeId: Int = AttributeImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      out.writeUTF(key)
      _next   .write(out)
      _hasNext.write(out)
      valid   .write(out)
      ex     .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      _next   .dispose()
      _hasNext.dispose()
      valid   .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit =
      valid() = false

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val v: LContext.Attribute.Value[A] = ctx.requestInput(LContext.Attribute[A](key))
      v.peer match {
        case Some(value) =>
          _next()     = value
          _hasNext()  = true

        case None =>
          _hasNext()  = false
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res     = _next()
      _hasNext()  = false
      res
    }
  }
}
