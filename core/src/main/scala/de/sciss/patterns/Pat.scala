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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.Aux
import de.sciss.patterns.graph.{Constant, LoopWithIndex, PatSeq}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

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

  def read[A](in: DataInput): Pat[A] = serializer[A].read(in)

  implicit def serializer[A]: ImmutableSerializer[Pat[A]] = anySer.asInstanceOf[ImmutableSerializer[Pat[A]]]

  private object anySer extends ImmutableSerializer[Pat[_]] {
    private final val SER_VERSION = 0x5347

    def write(v: Pat[_], out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      PatElem.write(v, out)
    }

    def read(in: DataInput): Pat[_] = {
      val cookie  = in.readShort()
      require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
      val res2  = PatElem.read[Pat[_]](in)
      res2
    }
  }

  //  var COUNT = 0
}

trait ProductWithAux extends Product {
  private[patterns] def aux: List[Aux]
}

trait Pat[+A] extends ProductWithAux {
//  Pat.COUNT += 1

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A]

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A]
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
abstract class Pattern[+A] extends Pat[A] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val _ref = new AnyRef

//  private[patterns] final def classTag[Tx]: ClassTag[Out[Tx]] = ClassTag(classOf[Out[Tx]])

  Graph.builder.addPattern(this)

  private[patterns] final def ref: AnyRef = _ref

  // default is _no aux_
  private[patterns] def aux: List[Aux] = Nil

//  /** A final implementation of this method which looks up the current stream graph
//    * builder and then performs the expansion just as `force`, returning the
//    * expanded object
//    *
//    * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
//    *          or a single stream, or a group of streams)
//    */
//  final private[patterns] def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S#Tx, A] = {
////    ctx.visit(ref, iterator)
//    ctx.addStream(_ref, iterator)
//  }

//  final private[patterns] def reset[Tx]()(implicit ctx: Context[S], tx: S#Tx): Unit =
//    ctx.getStreams(_ref).foreach(_.reset())
//
//  final def embed[Tx](implicit ctx: Context[S], tx: S#Tx): Stream[S#Tx, A] = iterator
}