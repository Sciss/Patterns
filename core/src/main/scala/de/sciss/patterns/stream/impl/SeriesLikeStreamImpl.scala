/*
 *  SeriesLikeStreamImpl.scala
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
package impl

import de.sciss.lucre.aux.Aux.{Num, Widen2}
import de.sciss.lucre.stm.Base
import de.sciss.serial.DataOutput

abstract class SeriesLikeStreamImpl[S <: Base[S], A1, A2, A] extends Stream[S, A] {

  protected val id          : S#Id
  protected val startStream : Stream[S, A1]
  protected val stepStream  : Stream[S, A2]
  protected val state       : S#Var[A]
  protected val _hasNext    : S#Var[Boolean]
  protected val valid       : S#Var[Boolean]

  implicit protected val widen: Widen2[A1, A2, A]
  implicit protected val num  : Num[A]

  protected def op(a: A, b: A): A

  import widen._

  final protected def writeData(out: DataOutput): Unit = {
    id          .write(out)
    startStream .write(out)
    stepStream  .write(out)
    state       .write(out)
    _hasNext    .write(out)
    valid       .write(out)
//    Aux.write(out, num  )
//    Aux.write(out, widen)
    num         .write(out)
    widen       .write(out)
  }

  final def dispose()(implicit tx: S#Tx): Unit = {
    id         .dispose()
    startStream.dispose()
    stepStream .dispose()
    state      .dispose()
    _hasNext   .dispose()
    valid      .dispose()
  }

  final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
    validate()
    _hasNext()
  }

  final def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
    startStream .reset()
    stepStream  .reset()
  }

  private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
//      count()     = 0
    _hasNext()  = startStream.hasNext // && lengthStream.hasNext
    if (_hasNext()) {
      state()   = widen1(startStream.next())
//        lengthVal() = lengthStream.next()
    }
  }

  final def next()(implicit ctx: Context[S], tx: S#Tx): A = {
    if (!hasNext) Stream.exhausted()
    val res = state()
//    val c   = count() + 1
//    count() = c
    _hasNext() = stepStream.hasNext // && c < lengthVal()
    if (_hasNext()) {
      state() = op(res, widen2(stepStream.next()))
    }
    res
  }
}