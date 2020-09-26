/*
 *  FolderCollectImpl.scala
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

import de.sciss.lucre.{Adjunct, Exec, Ident, Txn, Var, Folder => LFolder, Obj => LObj}
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.tailrec

object FolderCollectImpl extends StreamFactory {
  final val typeId = 0x466C436C // "FlCl"

  def expand[T <: Exec[T], A](pat: graph.Folder.Collect[A])
                             (implicit ctx: Context[T], tx: T, ex: Obj.Adjunct[A]): Stream[T, A] = {
    import pat.key
    val id        = tx.newId()
    val index     = id.newIntVar(0)
    val state     = PatElem.makeVar[T, A](id)
    val _hasNext  = id.newBooleanVar(false)
    val valid     = id.newBooleanVar(false)

    new StreamImpl[T, A](id = id, key = key, index = index, state = state,
      _hasNext = _hasNext, valid = valid, ex = ex)
  }

  def readIdentified[T <: Exec[T]](in: DataInput)
                                  (implicit ctx: Context[T], tx: T): Stream[T, Any] = {
    val id        = tx.readId(in)
    val key       = in.readUTF()
    val index     = id.readIntVar           (in)
    val state     = PatElem.readVar[T, Any] (id, in)
    val _hasNext  = id.readBooleanVar       (in)
    val valid     = id.readBooleanVar       (in)
    val ex        = Adjunct.readT[Obj.Adjunct[Any]](in)

    new StreamImpl[T, Any](id = id, key = key, index = index, state = state,
      _hasNext = _hasNext, valid = valid, ex = ex)
  }

  private final class StreamImpl[T <: Exec[T], A](id       : Ident[T],
                                                  key      : String,
                                                  index    : Var[T, Int    ],
                                                  state    : Var[T, A      ],
                                                  _hasNext : Var[T, Boolean],
                                                  valid    : Var[T, Boolean],
                                                  ex       : Obj.Adjunct[A]
                                                 )
    extends Stream[T, A] {

    private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                      (implicit tx: T, txOut: Out): Stream[Out, A] = {
      val idOut       = txOut.newId()
      val indexOut    = idOut.newIntVar(index())
      val stateOut    = PatElem.copyVar[Out, A](idOut, state())
      val hasNextOut  = idOut.newBooleanVar(_hasNext())
      val validOut    = idOut.newBooleanVar(valid())

      new StreamImpl[Out, A](id = idOut, key = key, index = indexOut, state = stateOut,
        _hasNext = hasNextOut, valid = validOut, ex = ex)
    }

    protected def typeId: Int = AudioCueNumFramesImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id        .write(out)
      out.writeUTF(key)
      index     .write(out)
      state     .write(out)
      _hasNext  .write(out)
      valid     .write(out)
      ex        .write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      id        .dispose()
      index     .dispose()
      state     .dispose()
      _hasNext  .dispose()
      valid     .dispose()
    }

    def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
      index() = 0
//      keyStream.reset()
    }

    def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
      validate()
      _hasNext()
    }

    private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
      advance()
    }

    private[this] val txRef = new ThreadLocal[T]

    private object Extractor extends Obj.Extractor[A] {
      def extract[T1 <: Txn[T1]](obj: LObj[T1])(implicit tx: T1): Option[A] = obj match {
        case f: LFolder[T1] =>
          val tx1: T = txRef.get
          val sz  = f.size

          @tailrec
          def loop(i: Int): Option[A] =
            if (i >= sz) None else {
              val j = i + 1
              index.update(j)(tx1)
              // XXX TODO --- `get` is O(N), but we cannot use
              // iterator; the only alternative would be to complete
              // iterate once and then keep the extracted values.
              val opt = f.get(i).flatMap { child =>
                ex.extract(child)
              }
              if (opt.isDefined) opt else {
                loop(j)
              }
            }

          val i0 = index()(tx1)
          loop(i0)

        case _ => None
      }
    }

//    @tailrec
    private def advance()(implicit ctx: Context[T], tx: T): Unit = {
//      val i = index()
//      val hasKey = (i >= 0) || {
//        val khn = keyStream.hasNext
//        if (khn) {
//          val keyVal = keyStream.next()
//          key   () = keyVal
//          index () = 0
//        }
//        khn
//      }
//      if (hasKey) {
//        val keyVal = key()
        txRef.set(tx)
        val vf = ctx.requestInput(LContext.Attribute[A](key)(Extractor))
        vf.peer match {
          case Some(x) =>
            state()     = x
            _hasNext()  = true
          case None    =>
            _hasNext()  = false
//            index() = -1
//            advance()
        }
//      } else {
//        _hasNext() = false
//      }
    }

    def next()(implicit ctx: Context[T], tx: T): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}
