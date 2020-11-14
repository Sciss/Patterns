/*
 *  SortWith.scala
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
package graph

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.SortWithImpl

final case class SortWith[A](outer: Pat[Pat[A]], it: It[(A, A)], lt: Pat[Boolean])
  extends Pattern[Pat[A]] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    SortWithImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val ltT     = t(lt)
    if (outerT.eq(outer) && ltT.eq(lt)) this else {
      val (itT, ltT1) = it.replaceIn(ltT)
      copy(outer = outerT, it = itT, lt = ltT1)
    }
  }
}