/*
 *  Stream.scala
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

import de.sciss.lucre.stm.{Base, Disposable, Plain}
import de.sciss.patterns.impl.StreamSerializer
import de.sciss.patterns.stream.StreamFactory
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable, Writer}

import scala.collection.{AbstractIterator, mutable}

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  implicit def serializer[S <: Base[S], A](implicit ctx: Context[S]): Serializer[S#Tx, S#Acc, Stream[S, A]] =
    ctx.streamSerializer // new Ser[S, A] // anySer.asInstanceOf[Ser[S, A]]

  implicit def writer[S <: Base[S], A]: Writer[Stream[S, A]] = anyWriter.asInstanceOf[Writer[Stream[S, A]]]

  private object anyWriter extends Writer[Stream[Plain, Any]] {
    def write(v: Stream[Plain, Any], out: DataOutput): Unit = v.write(out)
  }

  def addFactory(f: StreamFactory): Unit =
    StreamSerializer.addFactory(f)

  def read[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    serializer[S, A].read(in, access)

//  private val anySer = new Ser[Plain, Any]

  def apply[S <: Base[S], A](elems: A*)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    stream.PatSeqImpl(elems)

  object Copy {
    def apply[In <: Base[In], Out <: Base[Out]](implicit txIn: In#Tx, txOut: Out#Tx,
                                                context: Context[Out]): Copy[In, Out] =
      new Impl[In, Out]

    private final class Impl[In <: Base[In], Out <: Base[Out]](implicit txIn: In#Tx, txOut: Out#Tx,
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

      def copyVar[A](id: Out#Id, in: In#Var[Stream[In, A]]): Out#Var[Stream[Out, A]] = {
        val inV   = in()
        val outV  = apply(inV)
        txOut.newVar(id, outV)
      }
    }
  }
  trait Copy[In <: Base[In], Out <: Base[Out]] {
    implicit def context: Context[Out]

    def apply[A](in: Stream[In, A]): Stream[Out, A]

    def copyVar[A](id: Out#Id, in: In#Var[Stream[In, A]]): Out#Var[Stream[Out, A]]
  }
}
abstract class Stream[S <: Base[S], +A] extends Writable with Disposable[S#Tx] { outer =>
//  Stream.COUNT += 1

  // ---- abstract ----

  def reset()(implicit tx: S#Tx): Unit

  def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean
  def next ()(implicit ctx: Context[S], tx: S#Tx): A

  protected def typeId: Int

  protected def writeData(out: DataOutput): Unit

  // ---- impl ----

  final def write(out: DataOutput): Unit = {
    out.writeInt(typeId)
    writeData(out)
  }

  final def isEmpty (implicit ctx: Context[S], tx: S#Tx): Boolean = !hasNext
  final def nonEmpty(implicit ctx: Context[S], tx: S#Tx): Boolean = hasNext

  /** Note: consumes the stream. */
  final def toIterator(implicit ctx: Context[S], tx: S#Tx): Iterator[A] = new AbstractIterator[A] {
    def hasNext: Boolean = outer.hasNext

    def next(): A = outer.next()
  }

  /** Note: consumes the stream. */
  final def toList(implicit ctx: Context[S], tx: S#Tx): List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  /** Note: consumes the stream. */
  final def toVector(implicit ctx: Context[S], tx: S#Tx): Vector[A] = {
    val b = Vector.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  /** Makes a deep copy of this stream, possibly translating it to a different system `Out`. */
  private[patterns] def copyStream[Out <: Base[Out]](c: Stream.Copy[S, Out])
                                                    (implicit tx: S#Tx, txOut: Out#Tx): Stream[Out, A]
}
