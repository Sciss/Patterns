/*
 *  ChooseImpl.scala
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

import de.sciss.lucre.{Exec, Ident, RandomObj, Var}
import de.sciss.patterns.graph.Choose
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object ChooseImpl extends StreamFactory {
  final val typeId = 0x43686F6F // "Choo"

  def expand[T <: Exec[T], A](pat: Choose[A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[T]
    val choice    = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)
    val r         = ctx.mkRandom(pat.ref)
    new StreamImpl[T, A](id = id, inStream = inStream, choice = choice, _hasNext = _hasNext, valid = valid)(r)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val inStream  = Stream.read[T, Any](in)
    val choice    = PatElem.readVar[T, Any](id, in)
    val _hasNext  = id.readBooleanVar(in)
    val valid     = id.readBooleanVar(in)
    val r         = RandomObj.read[T](in)

    new StreamImpl[T, Any](id = id, inStream = inStream, choice = choice, _hasNext = _hasNext, valid = valid)(r)
  }

  private final class StreamImpl[T <: Exec[T], A](
                                                   id      : Ident[T],
                                                   inStream: Stream[T, A],
                                                   choice  : Var[T, A],
                                                   _hasNext: Var[T, Boolean],
                                                   valid   : Var[T, Boolean]
  ) (
    implicit val r: RandomObj[T]
  )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val inStreamOut = c(inStream)
      val choiceOut   = PatElem.copyVar[Out, A](idOut, choice())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())
      val rOut        = r.copy[Out]()
      new StreamImpl[Out, A](id = idOut, inStream = inStreamOut, choice = choiceOut, _hasNext = hasNextOut, valid = validOut)(rOut)
    }

    protected def typeId: Int = ChooseImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      choice  .write(out)
      _hasNext.write(out)
      valid   .write(out)
      r       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id      .dispose()
      inStream.dispose()
      choice  .dispose()
      _hasNext.dispose()
      valid   .dispose()
      r       .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      val vec   = inStream.toVector
      if (vec.nonEmpty) {
        val idx     = r.nextInt(vec.size)
        choice()    = vec(idx)
        _hasNext()  = true
      } else {
        _hasNext()  = false
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = choice()
      _hasNext() = false
      res
    }
  }
}
