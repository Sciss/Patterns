/*
 *  Format.scala
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
import de.sciss.patterns.stream.FormatImpl

final case class Format(s: Pat[String], args: Pat[_]*) extends Pattern[String] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, String] =
    FormatImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[String] = {
    val sT    = t(s)
    val argsT = args.map(t(_))
    Format(sT, argsT: _*)
  }
}