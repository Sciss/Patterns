/*
 *  Format.scala
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

final case class Format(s: Pat[String], args: Pat[_]*) extends Pattern[String] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, String] =
    impl.FormatImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[String] = {
    val sT    = t(s)
    val argsT = args.map(t(_))
    Format(sT, argsT: _*)
  }
}