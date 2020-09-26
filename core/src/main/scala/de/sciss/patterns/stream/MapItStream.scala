/*
 *  MapItStream.scala
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
import de.sciss.serial.{DataInput, DataOutput}

object MapItStream extends StreamFactory {

  final val typeId = 0x4D704974 // "MpIt"

  def expand[T <: Exec[T], A](outer: Pat[Pat[A]], token: Int)
                             (implicit ctx: Context[T], tx: T): AdvanceItStream[T, A] = {
    val id          = tx.newId()
    val outerStream = outer.expand[T]
    val inStream    = id.newVar[Stream[T, A]](null)
    val hasIn       = id.newBooleanVar(false)
    val _hasNext    = id.newBooleanVar(false)
    val valid       = id.newIntVar(0)
    
    new Impl[T, A](id, token = token, outerStream = outerStream, inStream = inStream, hasIn = hasIn,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id          = tx.readId(in)
    val token       = in.readInt()
    val outerStream = Stream.read[T, Pat[Any]](in)
    val inStream    = id.readVar[Stream[T, Any]](in)
    val hasIn       = id.readBooleanVar(in)
    val _hasNext    = id.readBooleanVar(in)
    val valid       = id.readIntVar(in)

    val res = new Impl[T, Any](id, token = token, outerStream = outerStream, inStream = inStream, hasIn = hasIn,
      _hasNext = _hasNext, valid = valid)
    ctx.registerItStream(res)
    res
  }

  private final class Impl[T <: Exec[T], A](
                                            id          : Ident[T],
                                            val token   : Int,
                                            outerStream : Stream[T, Pat[A]],
                                            inStream    : Var[T, Stream[T, A]],
                                            hasIn       : Var[T, Boolean],
                                            _hasNext    : Var[T, Boolean],
                                            valid       : Var[T, Int]  // bit 0 - outer, bit 1 - inner
                                           )
    extends AdvanceItStream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut           = txOut.newId()
      val outerStreamOut  = c(outerStream)
      val inStreamOut     = c.copyVar(idOut, inStream)
      val hasInOut        = idOut.newBooleanVar(hasIn())
      val hasNextOut      = idOut.newBooleanVar(_hasNext())
      val validOut        = idOut.newIntVar(valid())

      new Impl[Out, A](idOut, token = token, outerStream = outerStreamOut, inStream = inStreamOut, hasIn = hasInOut,
        _hasNext = hasNextOut, valid = validOut)
    }

    protected def typeId: Int = MapItStream.typeId

    protected def writeData(out: DataOutput): Unit = {
      id          .write(out)
      out.writeInt(token)
      outerStream .write(out)
      inStream    .write(out)
      hasIn       .write(out)
      _hasNext    .write(out)
      valid       .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id          .dispose()
      outerStream .dispose()
      inStream    .dispose()
      hasIn       .dispose()
      _hasNext    .dispose()
      valid       .dispose()
    }

    // $COVERAGE-OFF$
    private[this] lazy val simpleString = s"MapItStream@${hashCode().toHexString}"

    override def toString: String = simpleString
    // $COVERAGE-ON$

    def advance()(implicit ctx: Context[T], tx: T): Unit = {
      valid()   = 0x03 // hasIn and _hasNext will be valid
      val ohn   = outerStream.hasNext
      // $COVERAGE-OFF$
      logStream(s"$simpleString.advance(): outerStream.hasNext = $ohn")
      // $COVERAGE-ON$
      if (ohn) {
        val inPat     = outerStream.next()
        val inValue   = inPat.expand
        val ihn       = inValue.hasNext
        // $COVERAGE-OFF$
        logStream(s"$simpleString.advance(): inValue.hasNext = $ihn")
        // $COVERAGE-ON$
        inStream()    = inValue
        hasIn()       = true
        _hasNext()    = ihn
      } else {
        hasIn()       = false
        _hasNext()    = false
      }
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = {
      val v0 = valid()
      if ((v0 & 0x01) == 0) {         // hasIn is invalid
        advance()                     // validate hasIn and _hasNext
      } else if ((v0 & 0x02) == 0) {  // hasIn is valid but _hasNext is invalid
        valid()     = 0x03            // _hasNext will be valid
        val inValue = inStream()
        val ihn     = inValue.hasNext
        _hasNext()  = ihn
      }
    }

    def resetOuter()(implicit tx: T): Unit = {
      val v0 = valid()
      val v1 = v0 & ~0x01 // invalidate hasIn
      if (v1 != v0) {
        valid() = v1
        //    logStream(s"$simpleString.resetOuter()")
          outerStream.reset()
      }
    }

    def reset()(implicit tx: T): Unit = {
      val v0 = valid()
      if ((v0 & 0x03) == 0x03) {
        val hi = hasIn()
        // $COVERAGE-OFF$
        logStream(s"$simpleString.reset(); hasIn = $hi")
        // $COVERAGE-ON$
        if (hi) {
          val inValue = inStream()
          inValue.reset()
          valid() = 0x01    // clear 0x02
        }
      }
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val in      = inStream()
      val res     = in.next()
      _hasNext()  = in.hasNext
      // $COVERAGE-OFF$
      logStream(s"$simpleString.next() = $res; hasNext = ${in.hasNext}")
      // $COVERAGE-ON$
      res
    }
  }
}
//trait MapItStream[T <: Exec[T], A] extends Stream[T, A] {
//  def resetOuter()(implicit tx: T): Unit
//
//  def advance()(implicit ctx: Context[T], tx: T): Unit
//}