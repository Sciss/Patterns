/*
 *  Pat.scala
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

import de.sciss.patterns
import de.sciss.patterns.Types.{DoubleSeqTop, DoubleTop, IntTop, Top}
import de.sciss.patterns.graph.SeqFill

object Pat {
  type Int        = Pat[IntTop        ]
  type Double     = Pat[DoubleTop     ]
  type DoubleSeq  = Pat[DoubleSeqTop  ]
  type Event      = Pat[patterns.Event]

  type $[T <: Top, A] = Pat[T { type Out = A }]

  def seqFill[T <: Top](n: Pat.Int)(body: Pat.Int => Pat[T]): Pat[T] = {
    // val i = Series(start = 0, step = 1).take(n)
    val inner = Graph {
      body(0) // XXX TODO
    }
    SeqFill(n, inner)
  }
}
trait Pat[T <: Top] extends Top {
  final type Out = Stream[T#Out] // Seq[T#Out]

  private[patterns] def expand(implicit ctx: Context): Stream[T#Out]

  def iterator(implicit ctx: Context): Stream[T#Out]
  def embed   (implicit ctx: Context): Stream[T#Out]
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
abstract class Pattern[T <: Top] extends Pat[T] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val ref = new AnyRef

  Graph.builder.addPattern(this)

  /** A final implementation of this method which looks up the current stream graph
    * builder and then performs the expansion just as `force`, returning the
    * expanded object
    *
    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
    *          or a single stream, or a group of streams)
    */
  final private[patterns] def expand(implicit ctx: Context): Stream[T#Out] = {
//    ctx.visit(ref, iterator)
    ctx.addStream(ref, iterator)
  }

  final private[patterns] def reset()(implicit ctx: Context): Unit =
    ctx.getStreams(ref).foreach(_.reset())

  final def embed(implicit ctx: Context): Stream[T#Out] = iterator
}