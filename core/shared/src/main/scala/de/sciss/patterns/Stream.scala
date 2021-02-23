/*
 *  Stream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.lucre.{Disposable, Exec, Ident, Plain, Var}
import de.sciss.patterns.impl.StreamFormat
import de.sciss.patterns.stream.StreamFactory
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable, Writer}

import scala.collection.{AbstractIterator, mutable}

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  implicit def format[T <: Exec[T], A](implicit ctx: Context[T]): TFormat[T, Stream[T, A]] =
    ctx.streamFormat // new Ser[T, A] // anySer.asInstanceOf[Ser[T, A]]

  implicit def writer[T <: Exec[T], A]: Writer[Stream[T, A]] = anyWriter.asInstanceOf[Writer[Stream[T, A]]]

  private object anyWriter extends Writer[Stream[Plain, Any]] {
    def write(v: Stream[Plain, Any], out: DataOutput): Unit = v.write(out)
  }

  def addFactory(f: StreamFactory): Unit =
    StreamFormat.addFactory(f)

  def read[T <: Exec[T], A](in: DataInput)(implicit ctx: Context[T], tx: T): Stream[T, A] =
    format[T, A].readT(in)

//  private val anySer = new Ser[Plain, Any]

  def apply[T <: Exec[T], A](elems: A*)(implicit ctx: Context[T], tx: T): Stream[T, A] =
    stream.PatSeqImpl(elems)

  object Copy {
    def apply[In <: Exec[In], Out <: Exec[Out]](implicit txIn: In, txOut: Out,
                                                context: Context[Out]): Copy[In, Out] =
      new Impl[In, Out]

    private final class Impl[In <: Exec[In], Out <: Exec[Out]](implicit txIn: In, txOut: Out,
                                                               val context: Context[Out])
      extends Copy[In, Out] {

      // copy is used within one transaction, so we can use a mutable map here
      private[this] val seen = mutable.Map.empty[Stream[In, _], Stream[Out, _]]

      def apply[A](in: Stream[In, A]): Stream[Out, A] = if (in == null) null else {
        val out = seen.getOrElseUpdate(in, {
          in.copyStream[Out](this)
        })
        out.asInstanceOf[Stream[Out, A]]
      }

      def copyVar[A](id: Ident[Out], in: Var[In, Stream[In, A]]): Var[Out, Stream[Out, A]] = {
        val inV   = in()
        val outV  = apply(inV)
        id.newVar(outV)
      }
    }
  }
  trait Copy[In <: Exec[In], Out <: Exec[Out]] {
    implicit def context: Context[Out]

    def apply[A](in: Stream[In, A]): Stream[Out, A]

    def copyVar[A](id: Ident[Out], in: Var[In, Stream[In, A]]): Var[Out, Stream[Out, A]]
  }
}
abstract class Stream[T <: Exec[T], +A] extends Writable with Disposable[T] { outer =>
//  Stream.COUNT += 1

  // ---- abstract ----

  def reset()(implicit tx: T): Unit

  def hasNext(implicit ctx: Context[T], tx: T): Boolean
  def next ()(implicit ctx: Context[T], tx: T): A

  protected def typeId: Int

  protected def writeData(out: DataOutput): Unit

  // ---- impl ----

  final def write(out: DataOutput): Unit = {
    out.writeInt(typeId)
    writeData(out)
  }

  final def isEmpty (implicit ctx: Context[T], tx: T): Boolean = !hasNext
  final def nonEmpty(implicit ctx: Context[T], tx: T): Boolean = hasNext

  /** Note: consumes the stream. */
  final def toIterator(implicit ctx: Context[T], tx: T): Iterator[A] = new AbstractIterator[A] {
    def hasNext: Boolean = outer.hasNext

    def next(): A = outer.next()
  }

  /** Note: consumes the stream. */
  final def toList(implicit ctx: Context[T], tx: T): List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  /** Note: consumes the stream. */
  final def toVector(implicit ctx: Context[T], tx: T): Vector[A] = {
    val b = Vector.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  /** Makes a deep copy of this stream, possibly translating it to a different system `Out`. */
  private[patterns] def copyStream[Out <: Exec[Out]](c: Stream.Copy[T, Out])
                                                    (implicit tx: T, txOut: Out): Stream[Out, A]
}
