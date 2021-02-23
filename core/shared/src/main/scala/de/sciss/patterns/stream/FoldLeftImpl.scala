/*
 *  FoldLeftImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.patterns.graph.{FoldLeft, It, Pat}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.patterns.Log.{stream => logStream}

object FoldLeftImpl extends StreamFactory {
  final val typeId = 0x466F6C4C // "FolL"

  def expand[T <: Exec[T], B, A](pat: FoldLeft[B, A])(implicit ctx: Context[T], tx: T): Stream[T, A] = {
    import pat._
    val id          = tx.newId()
    val outerStream = outer.expand[T]
    val result      = id.newVar[Stream[T, A]](null)
    val valid       = id.newBooleanVar(false)

    new StreamImpl[T, B, A](id = id, outerStream = outerStream, z = z,
      inTokenId = itIn.token, carryTokenId = itCarry.token, inner = inner, result = result, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val outerStream = Stream.read[T, Pat[Any]](in)
    val z           = Pat.read[Any](in)
    val inTokenId   = in.readInt()
    val carryTokenId= in.readInt()
    val inner       = Pat.read[Any](in)
    val result      = id.readVar[Stream[T, Any]](in)
    val valid       = id.readBooleanVar(in)

    new StreamImpl[T, Any, Any](id = id, outerStream = outerStream, z = z,
      inTokenId = inTokenId, carryTokenId = carryTokenId, inner = inner, result = result, valid = valid)
  }

  private final class StreamImpl[T <: Exec[T], B, A](
                                                      id          : Ident[T],
                                                      outerStream : Stream[T, Pat[B]],
                                                      z           : Pat[A],
                                                      inTokenId   : Int,
                                                      carryTokenId: Int,
                                                      inner       : Pat[A],
                                                      result      : Var[T, Stream[T, A]],
                                                      valid       : Var[T, Boolean]
                                                    )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut          = txOut.newId()
      val outerStreamOut = c(outerStream)
      val resultOut      = c.copyVar(idOut, result)
      val validOut       = idOut.newBooleanVar(valid())

      new StreamImpl[Out, B, A](id = idOut, outerStream = outerStreamOut, z = z,
        inTokenId = inTokenId, carryTokenId = carryTokenId, inner = inner, result = resultOut, valid = validOut)
    }

    protected def typeId: Int = FoldLeftImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      outerStream .write(out)
      val patS = Pat.format[A]
      patS.write(z, out)
      out.writeInt(inTokenId   )
      out.writeInt(carryTokenId)
      patS.write(inner, out)
      result      .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      outerStream .dispose()
      result      .dispose()
      valid       .dispose()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit =
      if (!valid()) {
        valid()      = true
        logStream.debug("FoldLeft.iterator.validate()")
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

        val res   = innerRewrite.expand[T]
        result() = res
      }

    def reset()(implicit tx: T): Unit = if (valid()) {
      valid() = false
      outerStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      val s = result()
      s.hasNext
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val s = result()
      val res = s.next()
      logStream.debug(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}
