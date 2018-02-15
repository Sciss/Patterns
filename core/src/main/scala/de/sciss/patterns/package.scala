/*
 *  package.scala
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

package de.sciss

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.patterns.graph.{Bind, Constant}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.language.implicitConversions

package object patterns {
  implicit def patOps      [A](p: Pat[A]     ): PatOps      [A] = new PatOps(p)
  implicit def patNestedOps[A](p: Pat[Pat[A]]): PatNestedOps[A] = new PatNestedOps(p)

  //  implicit def const[A, T <: Top](x: A)(implicit tpe: T { type Out = A }): Elem[T] = Const(x)

  implicit def constIntPat        (x: Int           ): Pat[Int          ] = Constant[Int          ](x)
  implicit def constIntSeqPat     (xs: Seq[Int]     ): Pat[Seq[Int]     ] = Constant[Seq[Int]     ](xs)
  implicit def constDoublePat     (x: Double        ): Pat[Double       ] = Constant[Double       ](x)
  implicit def constDoubleSeqPat  (xs: Seq[Double]  ): Pat[Seq[Double]  ] = Constant[Seq[Double]  ](xs)
  implicit def constBooleanPat    (x: Boolean       ): Pat[Boolean      ] = Constant[Boolean      ](x)
  implicit def constBooleanSeqPat (xs: Seq[Boolean] ): Pat[Seq[Boolean] ] = Constant[Seq[Boolean] ](xs)
  implicit def constStringPat     (x: String        ): Pat[String       ] = Constant[String       ](x)

  implicit def patSeq[A, T](xs: Seq[A])(implicit lift: A => Pat[T]): Seq[Pat[T]] =
    xs.map(lift)

  implicit def seqPat[A, T](xs: Seq[A])(implicit lift: A => Pat[T]): Pat[Pat[T]] =
    Pat(xs.map(lift): _*)

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'pattern' - ", Locale.US)

  var showGraphLog    = false
  var showStreamLog   = false
  var showControlLog  = false

  @elidable(CONFIG) private[patterns] def logStream(what: => String): Unit =
    if (showStreamLog) Console.out.println(s"${logHeader.format(new Date())}stream $what")

  @elidable(CONFIG) private[patterns] def logGraph(what: => String): Unit =
    if (showGraphLog) Console.out.println(s"${logHeader.format(new Date())}graph $what")

  /** A shortcut for `Bind` with time/value pairs. */
  def Output[A](delta: Pat[Double], value: Pat[A]): Pat[Event] =
    Bind(Event.keyDelta -> delta, Event.keyValue -> value)
}
