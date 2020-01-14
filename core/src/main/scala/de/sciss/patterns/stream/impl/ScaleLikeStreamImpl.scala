/*
 *  ScaleLikeStream.scala
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
package impl

import de.sciss.lucre.adjunct.Adjunct
import de.sciss.lucre.adjunct.Adjunct.Widen2
import de.sciss.lucre.stm.Base
import de.sciss.serial.DataOutput

abstract class ScaleLikeStreamImpl[S <: Base[S], A1, A2, A] extends Stream[S, A] {
  // ---- abstract ----

  protected val widen: Widen2[A1, A2, A]
  protected val num  : Adjunct

  import widen._

  protected def inStream   : Stream[S, A1]
  protected def inLoStream : Stream[S, A1]
  protected def inHiStream : Stream[S, A1]
  protected def outLoStream: Stream[S, A2]
  protected def outHiStream: Stream[S, A2]

  protected def calc(inVal: A, inLoVal: A, inHiVal: A, outLoVal: A, outHiVal: A): A

  // ---- impl ----

  protected def writeData(out: DataOutput): Unit = {
    inStream   .write(out)
    inLoStream .write(out)
    inHiStream .write(out)
    outLoStream.write(out)
    outHiStream.write(out)
    widen      .write(out)
    num        .write(out)
  }

  final def dispose()(implicit tx: S#Tx): Unit = {
    inStream   .dispose()
    inLoStream .dispose()
    inHiStream .dispose()
    outLoStream.dispose()
    outHiStream.dispose()
  }

  final def reset()(implicit tx: S#Tx): Unit = {
    inStream    .reset()
    inLoStream  .reset()
    inHiStream  .reset()
    outLoStream .reset()
    outHiStream .reset()
  }

  final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
    inStream   .hasNext &&
    inLoStream .hasNext &&
    inHiStream .hasNext &&
    outLoStream.hasNext &&
    outHiStream.hasNext

  final def next()(implicit ctx: Context[S], tx: S#Tx): A = {
    if (!hasNext) Stream.exhausted()
    val inVal     = widen1(inStream    .next())
    val inLoVal   = widen1(inLoStream  .next())
    val inHiVal   = widen1(inHiStream  .next())
    val outLoVal  = widen2(outLoStream .next())
    val outHiVal  = widen2(outHiStream .next())

    calc(inVal, inLoVal, inHiVal, outLoVal, outHiVal)
  }
}
