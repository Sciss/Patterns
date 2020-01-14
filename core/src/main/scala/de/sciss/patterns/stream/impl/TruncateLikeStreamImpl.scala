/*
 *  TruncateStream.scala
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

import de.sciss.lucre.stm.Base

abstract class TruncateLikeStreamImpl[S <: Base[S], A]
  extends Stream[S, A] {

  // ---- abstract ----

  protected def id        : S#Id
  protected def inStream  : Stream[S, A]
  protected def lenStream : Stream[S, Int]
  protected def _hasNext  : S#Var[Boolean]
  protected def valid     : S#Var[Boolean]

  protected def validateWithLen(n: Int)(implicit ctx: Context[S], tx: S#Tx): Boolean

  // ---- impl ----

  final def reset()(implicit tx: S#Tx): Unit = if (valid.swap(false)) {
    lenStream .reset()
    inStream  .reset()
  }

  private def validate()(implicit ctx: Context[S], tx: S#Tx): Unit = if (!valid.swap(true)) {
    val lhn = lenStream.hasNext
    if (lhn) {
      val lenVal = math.max(0, lenStream.next())
      _hasNext() = validateWithLen(lenVal)
    } else {
      _hasNext()  = false
    }
  }

  final def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
    validate()
    _hasNext()
  }
}
