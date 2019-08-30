/*
 *  MapItStream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.serial.{DataInput, DataOutput}

object MapItStream extends StreamFactory {

  final val typeId = 0x4D704974 // "MpIt"

  def expand[S <: Base[S], A](outer: Pat[Pat[A]], token: Int)
                             (implicit ctx: Context[S], tx: S#Tx): AdvanceItStream[S, A] = {
    val id          = tx.newId()
    val outerStream = outer.expand[S]
    val inStream    = tx.newVar[Stream[S, A]](id, null)
    val hasIn       = tx.newBooleanVar(id, false)
    val _hasNext    = tx.newBooleanVar(id, false)
    val valid       = tx.newIntVar    (id, 0)
    
    new Impl[S, A](id, token = token, outerStream = outerStream, inStream = inStream, hasIn = hasIn,
      _hasNext = _hasNext, valid = valid)
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val token       = in.readInt()
    val outerStream = Stream.read[S, Pat[Any]](in, access)
    val inStream    = tx.readVar[Stream[S, Any]](id, in)
    val hasIn       = tx.readBooleanVar(id, in)
    val _hasNext    = tx.readBooleanVar(id, in)
    val valid       = tx.readIntVar    (id, in)

    val res = new Impl[S, Any](id, token = token, outerStream = outerStream, inStream = inStream, hasIn = hasIn,
      _hasNext = _hasNext, valid = valid)
    ctx.registerItStream(res)
    res
  }

  private final class Impl[S <: Base[S], A](
                                            id          : S#Id,
                                            val token   : Int,
                                            outerStream : Stream[S, Pat[A]],
                                            inStream    : S#Var[Stream[S, A]],
                                            hasIn       : S#Var[Boolean],
                                            _hasNext    : S#Var[Boolean],
                                            valid       : S#Var[Int]  // bit 0 - outer, bit 1 - inner
                                           )
    extends AdvanceItStream[S, A] {

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

    def dispose()(implicit tx: S#Tx): Unit = {
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

    def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = {
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

    def resetOuter()(implicit tx: S#Tx): Unit = {
      val v0 = valid()
      val v1 = v0 & ~0x01 // invalidate hasIn
      if (v1 != v0) {
        valid() = v1
        //    logStream(s"$simpleString.resetOuter()")
          outerStream.reset()
      }
    }

    def reset()(implicit tx: S#Tx): Unit = {
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

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
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
//trait MapItStream[S <: Base[S], A] extends Stream[S, A] {
//  def resetOuter()(implicit tx: S#Tx): Unit
//
//  def advance()(implicit ctx: Context[S], tx: S#Tx): Unit
//}