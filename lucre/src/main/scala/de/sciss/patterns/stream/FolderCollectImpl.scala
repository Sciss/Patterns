/*
 *  FolderCollectImpl.scala
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

import de.sciss.lucre.aux.Aux
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object FolderCollectImpl extends StreamFactory {
  final val typeId = 0x466C436C // "FlCl"

  def expand[S <: Base[S], A](pat: graph.Folder.Collect[A])
                             (implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._
    val id        = tx.newId()
    val keyStream = ??? // in.key.expand[S]
    val _hasNext  = tx.newBooleanVar(id, false)
    val valid     = tx.newBooleanVar(id, false)
//    ctx.requestInput(Context.Input.Attribute(folder.key))

//    new StreamImpl[S, A](id = id, keyStream = keyStream, _hasNext = _hasNext, valid = valid)
    ???
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val keyStream = Stream.read[S, String](in, access)
    val key       = tx.readVar[String]      (id, in)
    val index     = tx.readIntVar           (id, in)
    val state     = PatElem.readVar[S, Any] (id, in)
    val _hasNext  = tx.readBooleanVar       (id, in)
    val valid     = tx.readBooleanVar       (id, in)
    val ex        = Aux.readT[graph.Obj.Aux[Any]](in)

    new StreamImpl[S, Any](id = id, keyStream = keyStream, key = key, index = index, state = state,
      _hasNext = _hasNext, valid = valid, ex = ex)
  }

  private final class StreamImpl[S <: Base[S], A](id       : S#Id,
                                                  keyStream: Stream[S, String],
                                                  key      : S#Var[String ],
                                                  index    : S#Var[Int    ],
                                                  state    : S#Var[A      ],
                                                  _hasNext : S#Var[Boolean],
                                                  valid    : S#Var[Boolean],
                                                  ex       : graph.Obj.Aux[A]
                                                 )
    extends Stream[S, A] {

    protected def typeId: Int = AudioCueNumFramesImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      keyStream .write(out)
      key       .write(out)
      index     .write(out)
      state     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      ex        .write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      keyStream .dispose()
      key       .dispose()
      index     .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
      keyStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
      advance()
    }

    private[this] val txRef = new ThreadLocal[S#Tx]

    private object Extractor extends graph.Obj.Extractor[A] {
      def extract[T <: Sys[T]](obj: stm.Obj[T])(implicit tx: T#Tx): Option[A] = obj match {
        case f: stm.Folder[T] =>
          val tx1: S#Tx = txRef.get
          val sz  = f.size

          @tailrec
          def loop(i: Int): Option[A] =
            if (i >= sz) None else {
              val opt = f.get(i).flatMap { child =>
                ex.extract(child)
              }
              if (opt.isDefined) opt else {
                val j = i + 1
                index.update(j)(tx1)
                loop(j)
              }
            }

          val i0 = index()(tx1)
          loop(i0)

        case _ => None
      }
    }

    @tailrec
    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val i = index()
      val hasKey = (i >= 0) || {
        val khn = keyStream.hasNext
        if (khn) {
          val keyVal = keyStream.next()
          key   () = keyVal
          index () = 0
        }
        khn
      }
      if (hasKey) {
        val keyVal = key()
        txRef.set(tx)
        val vf = ctx.requestInput(LContext.Attribute[A](keyVal)(Extractor))
        vf.peer match {
          case Some(x) =>
            state()     = x
            _hasNext()  = true
          case None    =>
            index() = -1
            advance()
        }
      } else {
        _hasNext() = false
      }
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}
