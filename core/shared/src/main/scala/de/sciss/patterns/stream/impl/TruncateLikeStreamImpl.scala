/*
 *  TruncateStream.scala
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

import de.sciss.lucre.{Exec, Ident, Var}

abstract class TruncateLikeStreamImpl[T <: Exec[T], A]
  extends Stream[T, A] {

  // ---- abstract ----

  protected def id        : Ident[T]
  protected def inStream  : Stream[T, A]
  protected def lenStream : Stream[T, Int]
  protected def _hasNext  : Var[T, Boolean]
  protected def valid     : Var[T, Boolean]

  protected def validateWithLen(n: Int)(implicit ctx: Context[T], tx: T): Boolean

  // ---- impl ----

  final def reset()(implicit tx: T): Unit = if (valid.swap(false)) {
    lenStream .reset()
    inStream  .reset()
  }

  private def validate()(implicit ctx: Context[T], tx: T): Unit = if (!valid.swap(true)) {
    val lhn = lenStream.hasNext
    if (lhn) {
      val lenVal = math.max(0, lenStream.next())
      _hasNext() = validateWithLen(lenVal)
    } else {
      _hasNext()  = false
    }
  }

  final def hasNext(implicit ctx: Context[T], tx: T): Boolean = {
    validate()
    _hasNext()
  }
}
