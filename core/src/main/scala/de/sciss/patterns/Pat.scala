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

import de.sciss.patterns.Types.Aux
import de.sciss.patterns.graph.{PatSeq, SeqFill}

object Pat {
  // XXX TODO -- this might be larger constrained than necessary
  // e.g. could be
  // type Int = Pat[CTop { type Out = Int }] ?
//  type Int        = Pat[IntTop        ]
//  type IntSeq     = Pat[IntSeqTop     ]
//  type Double     = Pat[DoubleTop     ]
//  type DoubleSeq  = Pat[DoubleSeqTop  ]
//  type Boolean    = Pat[BooleanTop    ]
//  type BooleanSeq = Pat[BooleanSeqTop ]
//  type String     = Pat[StringTop     ]
//  type Event      = Pat[patterns.Event]
//
//  type Tuple2[A <: Top, B <: Top] = Pat[Tuple2Top[A, B]]

  def Int    (elems: scala.Int*    ): Pat[Int]      = apply[Int    ](elems: _*)
  def Double (elems: scala.Double* ): Pat[Double]   = apply[Double ](elems: _*)
  def Boolean(elems: scala.Boolean*): Pat[Boolean]  = apply[Boolean](elems: _*)

//  type $[T <: Top, A] = Pat[T { type Out[Tx] = A }]

  def seqFill[A](n: Pat[Int])(body: Pat[Int] => Pat[A]): Pat[A] = {
    // val i = Series(start = 0, step = 1).take(n)
    val it    = Graph.builder.allocToken[Int]()
    val inner = Graph[A] {
      body(it) // XXX TODO
    }
    SeqFill(n, it, inner)
  }

  def apply[A](elems: A*): Pat[A] = PatSeq(elems: _*)
}

trait ProductWithAux extends Product {
  private[patterns] def aux: List[Aux]
}

trait Pat[+A] extends ProductWithAux {
  private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A]

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A]
  def embed   [Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A]

  def transform(t: Transform): Pat[A]

  private[patterns] def reset[Tx]()(implicit ctx: Context[Tx], tx: Tx): Unit
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
abstract class Pattern[+A] extends Pat[A] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val _ref = new AnyRef

//  private[patterns] final def classTag[Tx]: ClassTag[Out[Tx]] = ClassTag(classOf[Out[Tx]])

  Graph.builder.addPattern(this)

  protected final def ref: AnyRef = _ref

  // default is _no aux_
  private[patterns] def aux: List[Aux] = Nil

  /** A final implementation of this method which looks up the current stream graph
    * builder and then performs the expansion just as `force`, returning the
    * expanded object
    *
    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
    *          or a single stream, or a group of streams)
    */
  final private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
//    ctx.visit(ref, iterator)
    ctx.addStream(_ref, iterator)
  }

  final private[patterns] def reset[Tx]()(implicit ctx: Context[Tx], tx: Tx): Unit =
    ctx.getStreams(_ref).foreach(_.reset())

  final def embed[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = iterator
}