/*
 *  ChooseImpl.scala
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
package stream

import de.sciss.lucre.stm.{Base, TxnRandom}
import de.sciss.patterns.graph.Choose
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput}

object ChooseImpl extends StreamFactory {
  final val typeId = 0x43686F6F // "Choo"

  def expand[S <: Base[S], A](pat: Choose[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val inStream  = in.expand[S]
    val choice    = PatElem.makeVar[S, A](id)
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
    val r         = ctx.mkRandom(pat.ref)

    new StreamImpl[S, A](id = id, inStream = inStream, choice = choice, _hasNext = _hasNext, valid = valid)(r)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, Any](in, access)
    val choice    = PatElem.readVar[S, Any](id, in)
    val _hasNext  = tx.readBooleanVar(id, in)
    val valid     = tx.readBooleanVar(id, in)
    val r         = TxnRandom.read[S](in, access)

    new StreamImpl[S, Any](id = id, inStream = inStream, choice = choice, _hasNext = _hasNext, valid = valid)(r)
  }

  private final class StreamImpl[S <: Base[S], A](
                                                   id      : S#Id,
                                                   inStream: Stream[S, A],
                                                   choice  : S#Var[A],
                                                   _hasNext: S#Var[Boolean],
                                                   valid   : S#Var[Boolean]
  ) (
    implicit val r: TxnRandom[S]
  )
    extends Stream[S, A] {

    protected def typeId: Int = ChooseImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      choice  .write(out)
      _hasNext.write(out)
      valid   .write(out)
      r       .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      choice  .dispose()
      _hasNext.dispose()
      valid   .dispose()
      r       .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      inStream.reset()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      val vec   = inStream.toVector
      if (vec.nonEmpty) {
        val idx     = r.nextInt(vec.size)
        choice()    = vec(idx)
        _hasNext()  = true
      } else {
        _hasNext()  = false
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = choice()
      _hasNext() = false
      res
    }
  }
}
