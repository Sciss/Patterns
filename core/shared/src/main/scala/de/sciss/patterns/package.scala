/*
 *  package.scala
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

package de.sciss

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.patterns.graph.{Bind, Constant}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.language.implicitConversions

package object patterns {
  implicit def patOps      [A]    (p: Pat[A]     ): PatOps      [A]     = new PatOps(p)
  implicit def patNestedOps[A]    (p: Pat[Pat[A]]): PatNestedOps[A]     = new PatNestedOps(p)
  implicit def patTuple2Ops[A, B] (p: Pat[(A, B)]): PatTuple2Ops[A, B]  = new PatTuple2Ops(p)
  implicit def seqToPatOps [A]    (xs: Seq[A])    : SeqToPatOps [A]     = new SeqToPatOps(xs)

  implicit def constIntPat        (x: Int           ): Pat[Int          ] = Constant[Int          ](x)
  implicit def constDoublePat     (x: Double        ): Pat[Double       ] = Constant[Double       ](x)
  implicit def constBooleanPat    (x: Boolean       ): Pat[Boolean      ] = Constant[Boolean      ](x)
  implicit def constStringPat     (x: String        ): Pat[String       ] = Constant[String       ](x)

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'pattern' - ", Locale.US)

  var showStreamLog   = false

  @elidable(CONFIG) private[patterns] def logStream(what: => String): Unit =
    if (showStreamLog) Console.out.println(s"${logHeader.format(new Date())}stream $what")

  /** A shortcut for `Bind` with time/value pairs. */
  def Output[A](delta: Pat[Double], value: Pat[A]): Pat[Event] =
    Bind(Event.keyDelta -> delta, Event.keyValue -> value)
}
