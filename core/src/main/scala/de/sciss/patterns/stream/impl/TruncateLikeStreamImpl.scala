/*
 *  TruncateStream.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns

abstract class TruncateLikeStreamImpl[S <: Base[S], A]
  extends Stream[S, A] {

  // ---- abstract ----

  protected def id        : S#Id
  protected def inStream  : patterns.Stream[S, A]
  protected def lenStream : patterns.Stream[S, Int]
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
