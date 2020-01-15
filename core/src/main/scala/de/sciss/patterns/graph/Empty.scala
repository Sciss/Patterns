/*
 *  Empty.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.EmptyImpl

final case class Empty() extends Pat[Nothing] { pat =>
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Nothing] =
    EmptyImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Nothing] = this
}