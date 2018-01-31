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
import de.sciss.patterns.Types.{Aux, BooleanSeqTop, BooleanTop, CTop, DoubleSeqTop, DoubleTop, IntSeqTop, IntTop, Top}
import de.sciss.patterns.graph.{PatSeq, SeqFill}

object Pat {
  // XXX TODO -- this might be larger constrained than necessary
  // e.g. could be
  // type Int = Pat[CTop { type Out = Int }] ?
  type Int        = Pat[IntTop        ]
  type IntSeq     = Pat[IntSeqTop     ]
  type Double     = Pat[DoubleTop     ]
  type DoubleSeq  = Pat[DoubleSeqTop  ]
  type Boolean    = Pat[BooleanTop    ]
  type BooleanSeq = Pat[BooleanSeqTop ]
  type Event      = Pat[patterns.Event]

  def Int    (elems: scala.Int*    ): Pat.Int     = apply[IntTop    ](elems: _*)
  def Double (elems: scala.Double* ): Pat.Double  = apply[DoubleTop ](elems: _*)
  def Boolean(elems: scala.Boolean*): Pat.Boolean = apply[BooleanTop](elems: _*)

  type $[T <: Top, A] = Pat[T { type Out[Tx] = A }]

  def seqFill[T <: Top](n: Pat.Int)(body: Pat.Int => Pat[T]): Pat[T] = {
    // val i = Series(start = 0, step = 1).take(n)
    val it    = Graph.builder.allocToken[IntTop]()
    val inner = Graph {
      body(it) // XXX TODO
    }
    SeqFill(n, inner, it)
  }

  def apply[T <: CTop](elems: T#COut*): Pat[T] = PatSeq(elems)
}

trait ProductWithAux extends Product {
  private[patterns] def aux: List[Aux]
}

trait Pat[T <: Top] extends Top with ProductWithAux {
//  final type Out[Tx] = Stream[Tx, T#Out] // Seq[T#Out]
  final type Out[Tx] = Stream[Tx, T#Out[Tx]] // Seq[T#Out]
//  final type Out = Pat[T]

  private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]]

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]]
  def embed   [Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]]
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
abstract class Pattern[T <: Top] extends Pat[T] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val ref = new AnyRef

  Graph.builder.addPattern(this)

  // default is _no aux_
  private[patterns] def aux: List[Aux] = Nil

  /** A final implementation of this method which looks up the current stream graph
    * builder and then performs the expansion just as `force`, returning the
    * expanded object
    *
    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
    *          or a single stream, or a group of streams)
    */
  final private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = {
//    ctx.visit(ref, iterator)
    ctx.addStream(ref, iterator)
  }

  final private[patterns] def reset[Tx]()(implicit ctx: Context[Tx], tx: Tx): Unit =
    ctx.getStreams(ref).foreach(_.reset())

  final def embed[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = iterator
}