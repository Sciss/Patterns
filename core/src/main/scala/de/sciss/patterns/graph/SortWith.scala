/*
 *  SortWith.scala
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
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.SortWithImpl

final case class SortWith[A](outer: Pat[Pat[A]], it: It[(A, A)], lt: Pat[Boolean])
  extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    SortWithImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val ltT     = t(lt)
    if (outerT.eq(outer) && ltT.eq(lt)) this else {
      val (itT, ltT1) = it.replaceIn(ltT)
      copy(outer = outerT, it = itT, lt = ltT1)
    }
  }
}