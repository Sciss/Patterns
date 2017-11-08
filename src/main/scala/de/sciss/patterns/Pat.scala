/*
 *  Pat.scala
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

package de.sciss.patterns

import de.sciss.patterns.Types.{DoubleTop, IntTop, Top}

object Pat {
  type Int    = Pat[IntTop    ]
  type Double = Pat[DoubleTop ]

  type $[T <: Top, A] = Pat[T { type Out = A }]
}
trait Pat[T <: Top] {
  val tpe: T

  private[patterns] def expand(implicit ctx: Context): Iterator[tpe.Out]

  def iterator(implicit ctx: Context): Iterator[tpe.Out]
  def embed   (implicit ctx: Context): Iterator[tpe.Out]
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
trait Pattern[T <: Top] extends Pat[T] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val ref = new AnyRef

  /** A final implementation of this method which looks up the current stream graph
    * builder and then performs the expansion just as `force`, returning the
    * expanded object
    *
    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
    *          or a single stream, or a group of streams)
    */
  final private[patterns] def expand(implicit ctx: Context): Iterator[tpe.Out] = ctx.visit(ref, iterator)

  final def embed(implicit ctx: Context): Iterator[tpe.Out] = iterator
}