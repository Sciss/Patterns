/*
 *  SeriesLikeStreamImpl.scala
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
package impl

import de.sciss.lucre.Adjunct.{Num, Widen2}
import de.sciss.lucre.{Exec, Ident, Var}
import de.sciss.serial.DataOutput

abstract class SeriesLikeStreamImpl[T <: Exec[T], A1, A2, A] extends Stream[T, A] {

  protected val id          : Ident[T]
  protected val startStream : Stream[T, A1]
  protected val stepStream  : Stream[T, A2]
  protected val state       : Var[T, A]
  protected val _hasNext    : Var[T, Boolean]
  protected val valid       : Var[T, Boolean]

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
//    Adjunct.write(out, num  )
//    Adjunct.write(out, widen)
    num         .write(out)
    widen       .write(out)
  }

  final def dispose()(implicit tx: T): Unit = {
    id         .dispose()
    startStream.dispose()
    stepStream .dispose()
    state      .dispose()
    _hasNext   .dispose()
    valid      .dispose()
  }

  final def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
    validate()
    _hasNext()
  }

  final def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
    startStream .reset()
    stepStream  .reset()
  }

  private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
//      count()     = 0
    _hasNext()  = startStream.hasNext // && lengthStream.hasNext
    if (_hasNext()) {
      state()   = widen1(startStream.next())
//        lengthVal() = lengthStream.next()
    }
  }

  final def next()(implicit ctx: Context[T], tx: T): A = {
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