/*
 *  package.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG

package object patterns {
//  implicit def geOps1      (g: PE    ): PEOps1 = new PEOps1(g)
//  implicit def geOps2      (g: PE    ): PEOps2 = new PEOps2(g)
//  implicit def intGeOps2   (i: Int   ): PEOps2 = new PEOps2(i)
//  implicit def doubleGeOps2(d: Double): PEOps2 = new PEOps2(d)

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'pattern' - ", Locale.US)

  var showGraphLog    = false
  var showStreamLog   = false
  var showControlLog  = false

  @elidable(CONFIG) private[patterns] def logStream(what: => String): Unit =
    if (showStreamLog) Console.out.println(s"${logHeader.format(new Date())}stream $what")

  @elidable(CONFIG) private[patterns] def logGraph(what: => String): Unit =
    if (showGraphLog) Console.out.println(s"${logHeader.format(new Date())}graph $what")
}
