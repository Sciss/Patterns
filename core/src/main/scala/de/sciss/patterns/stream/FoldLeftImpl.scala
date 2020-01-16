/*
 *  FoldLeftImpl.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.{FoldLeft, It}
import de.sciss.serial.{DataInput, DataOutput}

object FoldLeftImpl extends StreamFactory {
  final val typeId = 0x466F6C4C // "FolL"

  def expand[S <: Base[S], B, A](pat: FoldLeft[B, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id          = tx.newId()
    val outerStream = outer.expand[S]
    val result      = tx.newVar[Stream[S, A]](id, null)
    val valid       = tx.newBooleanVar(id, false)

    new StreamImpl[S, B, A](id = id, outerStream = outerStream, z = z,
      inTokenId = itIn.token, carryTokenId = itCarry.token, inner = inner, result = result, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val outerStream = Stream.read[S, Pat[Any]](in, access)
    val z           = Pat.read[Any](in)
    val inTokenId   = in.readInt()
    val carryTokenId= in.readInt()
    val inner       = Pat.read[Any](in)
    val result      = tx.readVar[Stream[S, Any]](id, in)
    val valid       = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any, Any](id = id, outerStream = outerStream, z = z,
      inTokenId = inTokenId, carryTokenId = carryTokenId, inner = inner, result = result, valid = valid)
  }

  private final class StreamImpl[S <: Base[S], B, A](
                                                    id          : S#Id,
                                                    outerStream : Stream[S, Pat[B]],
                                                    z           : Pat[A],
                                                    inTokenId   : Int,
                                                    carryTokenId: Int,
                                                    inner       : Pat[A],
                                                    result      : S#Var[Stream[S, A]],
                                                    valid       : S#Var[Boolean]
                                                    )
    extends Stream[S, A] {

    private[patterns] def copyStream[Out <: Base[Out]]()(implicit tx: S#Tx, txOut: Out#Tx,
                                                         ctx: Context[Out]): Stream[Out, A] = {
      val idOut          = txOut.newId()
      val outerStreamOut = outerStream.copyStream[Out]()
      val resultOut      = {
        val r = result()
        val rOut = if (r == null) null else r.copyStream[Out]()
        txOut.newVar[Stream[Out, A]](idOut, rOut)
      }
      val validOut       = txOut.newBooleanVar(idOut, valid())

      new StreamImpl[Out, B, A](id = idOut, outerStream = outerStreamOut, z = z,
        inTokenId = inTokenId, carryTokenId = carryTokenId, inner = inner, result = resultOut, valid = validOut)
    }

    protected def typeId: Int = FoldLeftImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      outerStream .write(out)
      val patS = Pat.serializer[A]
      patS.write(z, out)
      out.writeInt(inTokenId   )
      out.writeInt(carryTokenId)
      patS.write(inner, out)
      result      .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id          .dispose()
      outerStream .dispose()
      result      .dispose()
      valid       .dispose()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit =
      if (!valid()) {
        valid()      = true
        logStream("FoldLeft.iterator.validate()")
        val _outer        = outerStream.toVector
        val innerRewrite  = _outer.foldLeft(z) { (y: Pat[A], x: Pat[B]) =>
          val t = new Transform {
            def applyOne[X](in: Pat[X]): Pat[X] = in match {
              case It(`inTokenId`)    => x.asInstanceOf[Pat[X]]
              case It(`carryTokenId`) => y.asInstanceOf[Pat[X]]
              case other              => other
            }
          }
          t(inner)
        }

        val res   = innerRewrite.expand[S]
        result() = res
      }

    def reset()(implicit tx: S#Tx): Unit = if (valid()) {
      valid() = false
      outerStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      val s = result()
      s.hasNext
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val s = result()
      val res = s.next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}
