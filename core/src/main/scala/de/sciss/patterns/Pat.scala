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
import de.sciss.patterns.graph.{Constant, LoopWithIndex, PatSeq}

object Pat {
//  def Int    (elems: scala.Int*    ): Pat[Int]      = apply[Int    ](elems: _*)
//  def Double (elems: scala.Double* ): Pat[Double]   = apply[Double ](elems: _*)
//  def Boolean(elems: scala.Boolean*): Pat[Boolean]  = apply[Boolean](elems: _*)
//
//  def loop[A](body: => Pat[A]): Pat[A] = loop[A](scala.Int.MaxValue)(body)

  def loop[A](n: Pat[Int] = scala.Int.MaxValue)(body: => Pat[A]): Pat[A] = loopWithIndex(n)(_ => body)

  def loopWithIndex[A](n: Pat[Int] = scala.Int.MaxValue)(body: Pat[Int] => Pat[A]): Pat[A] = {
    val b     = Graph.builder
    val it    = b.allocToken[Int]()
//    val level = b.level + 1
    val inner = Graph {
      body(it)
    }
    LoopWithIndex(n = n, it = it, inner = inner /* , innerLevel = level */)
  }

  def apply[A](elems: A*): Pat[A] = PatSeq(elems: _*)

  def fold[A](in: Pat[A], n: Pat[Int])(fun: Pat[A] => Pat[A]): Pat[A] = {
    // XXX TODO --- introduce an optimised version of this
    Constant(Pat(0)).take(n).foldLeft(in)((y, _) => fun(y))
  }

  //  var COUNT = 0
}

trait ProductWithAux extends Product {
  private[patterns] def aux: List[Aux]
}

trait Pat[+A] extends ProductWithAux {
//  Pat.COUNT += 1

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A]

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A]
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

//  /** A final implementation of this method which looks up the current stream graph
//    * builder and then performs the expansion just as `force`, returning the
//    * expanded object
//    *
//    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
//    *          or a single stream, or a group of streams)
//    */
//  final private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
////    ctx.visit(ref, iterator)
//    ctx.addStream(_ref, iterator)
//  }

//  final private[patterns] def reset[Tx]()(implicit ctx: Context[Tx], tx: Tx): Unit =
//    ctx.getStreams(_ref).foreach(_.reset())
//
//  final def embed[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = iterator
}