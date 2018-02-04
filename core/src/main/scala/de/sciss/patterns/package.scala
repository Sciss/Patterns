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

import de.sciss.patterns.Types.{BooleanSeqTop, BooleanTop, DoubleSeqTop, DoubleTop, IntSeqTop, IntTop, StringTop, Top}
import de.sciss.patterns.graph.{Bind, Constant, PatPat}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.language.implicitConversions

package object patterns {
  implicit def patOps      [T <: Top](p: Pat[T]     ): PatOps      [T] = new PatOps(p)
  implicit def patNestedOps[T <: Top](p: Pat[Pat[T]]): PatNestedOps[T] = new PatNestedOps(p)

  //  implicit def const[A, T <: Top](x: A)(implicit tpe: T { type Out = A }): Elem[T] = Const(x)

  implicit def constIntPat        (x: Int           ): Pat[IntTop       ] = Constant[IntTop       ](x)
  implicit def constIntSeqPat     (xs: Seq[Int]     ): Pat[IntSeqTop    ] = Constant[IntSeqTop    ](xs)
  implicit def constDoublePat     (x: Double        ): Pat[DoubleTop    ] = Constant[DoubleTop    ](x)
  implicit def constDoubleSeqPat  (xs: Seq[Double]  ): Pat[DoubleSeqTop ] = Constant[DoubleSeqTop ](xs)
  implicit def constBooleanPat    (x: Boolean       ): Pat[BooleanTop   ] = Constant[BooleanTop   ](x)
  implicit def constBooleanSeqPat (xs: Seq[Boolean] ): Pat[BooleanSeqTop] = Constant[BooleanSeqTop](xs)
  implicit def constStringPat     (x: String        ): Pat[StringTop    ] = Constant[StringTop    ](x)

  implicit def patSeq[A, T <: Top](xs: Seq[A])(implicit lift: A => Pat[T]): Seq[Pat[T]] =
    xs.map(lift)

  implicit def seqPat[A, T <: Top](xs: Seq[A])(implicit lift: A => Pat[T]): Pat[Pat[T]] =
    PatPat(xs.map(lift))

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'pattern' - ", Locale.US)

  var showGraphLog    = false
  var showStreamLog   = false
  var showControlLog  = false

  @elidable(CONFIG) private[patterns] def logStream(what: => String): Unit =
    if (showStreamLog) Console.out.println(s"${logHeader.format(new Date())}stream $what")

  @elidable(CONFIG) private[patterns] def logGraph(what: => String): Unit =
    if (showGraphLog) Console.out.println(s"${logHeader.format(new Date())}graph $what")

  /** A shortcut for `Bind` with time/value pairs. */
  def Output[T <: Top](delta: Pat.Double, value: Pat[T]): Pat.Event =
    Bind(Event.keyDelta -> delta, Event.keyValue -> value)
}
