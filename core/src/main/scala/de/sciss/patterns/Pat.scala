/*
 *  Pat.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.lucre.Exec
import de.sciss.patterns.graph.{Constant, LoopWithIndex, PatSeq}
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.{DataInput, DataOutput, ConstFormat}

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

  def read[A](in: DataInput): Pat[A] = format[A].read(in)

  def write[A](pat: Pat[A], out: DataOutput): Unit = format[A].write(pat, out)

  implicit def format[A]: ConstFormat[Pat[A]] = anyFmt.asInstanceOf[ConstFormat[Pat[A]]]

  private object anyFmt extends ConstFormat[Pat[_]] {
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

//  /** Note: this is not particularly efficient, as we build an entirely redundant seq */
//  implicit def cbf[A]: CanBuildFrom[Nothing, A, Pat[A]] = anyCBF.asInstanceOf[CanBuildFrom[Nothing, A, Pat[A]]]
//
//  private object anyCBF extends CanBuildFrom[Any, Any, Pat[Any]] {
//    def apply(from: Any): mutable.Builder[Any, Pat[Any]] = apply()
//
//    def apply(): mutable.Builder[Any, Pat[Any]] = new mutable.Builder[Any, Pat[Any]] {
//      private[this] val peer = Seq.newBuilder[Any]
//
//      def +=(elem: Any): this.type = {
//        peer += elem
//        this
//      }
//
//      def clear(): Unit = peer.clear()
//
//      def result(): Pat[Any] = Pat(peer.result(): _*)
//    }
//  }

  //  var COUNT = 0
}

/** The main trait used to define pattern elements.
  *
  * A lot of operations on `Pat` are defined separately in `PatOps`.
  *
  * @see  [[PatOps]]
  */
trait Pat[+A] extends Product {
//  Pat.COUNT += 1

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A]

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A]
}

/** A pattern is a pattern element (`Pat`) that caches it's iterator expansion. */
abstract class Pattern[+A] extends Pat[A] {
  // this acts now as a fast unique reference
  @transient final private[this] lazy val _ref = new AnyRef

  private[patterns] final def ref: AnyRef = _ref
}