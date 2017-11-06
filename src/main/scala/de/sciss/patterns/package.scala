package de.sciss

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.language.implicitConversions

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
