/*
 *  PatImport.scala
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

import de.sciss.patterns.graph.{Bind, Constant, Pat}

import scala.language.implicitConversions

object PatImport extends PatImport
trait PatImport {
  implicit def seqToPatOps [A]    (xs: Seq[A])    : SeqToPatOps [A]     = new SeqToPatOps(xs)
  implicit def constIntPat        (x: Int           ): Pat[Int          ] = Constant[Int          ](x)
  implicit def constDoublePat     (x: Double        ): Pat[Double       ] = Constant[Double       ](x)
  implicit def constBooleanPat    (x: Boolean       ): Pat[Boolean      ] = Constant[Boolean      ](x)
  implicit def constStringPat     (x: String        ): Pat[String       ] = Constant[String       ](x)

  /** A shortcut for `Bind` with time/value pairs. */
  def Output[A](delta: Pat[Double], value: Pat[A]): Pat[Event] =
    Bind(Event.keyDelta -> delta, Event.keyValue -> value)
}
