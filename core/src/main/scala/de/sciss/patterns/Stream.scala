/*
 *  Stream.scala
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

import de.sciss.lucre.stm.{Base, Disposable, Plain}
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

import scala.annotation.tailrec

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  def fill[S <: Base[S], A](n: Int)(elem: => A)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    continually(elem).take(n) // XXX TODO -- more efficient

  def empty[S <: Base[S], A]: Stream[S, A] = new Stream[S, A] {
    override def toString = "Stream.empty"

    protected def typeId: Int = 0x456D7074

    protected def writeData(out: DataOutput): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = ()

    def reset()(implicit tx: S#Tx): Unit = ()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = false
    def next ()(implicit ctx: Context[S], tx: S#Tx): A       = Stream.exhausted()
  }

  def continually[S <: Base[S], A](elem: => A): Stream[S, A] = new Stream[S, A] {
    override def toString = s"Stream.continually@${hashCode().toHexString}"

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = ()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = true
    def next ()(implicit ctx: Context[S], tx: S#Tx): A       = elem
  }

  def single[S <: Base[S], A](elem: A)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    new Single[S, A](elem, tx)

  implicit def serializer[S <: Base[S], A]: Serializer[S#Tx, S#Acc, Stream[S, A]] = anySer.asInstanceOf[Ser[S, A]]

  def read[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] =
    serializer[S, A].read(in, access)

  private val anySer = new Ser[Plain, Any]

  private final class Ser[S <: Base[S], A] extends Serializer[S#Tx, S#Acc, Stream[S, A]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = ???

    def write(v: Stream[S, A], out: DataOutput): Unit = ???
  }

  private final class Single[S <: Base[S], A](elem: A, tx0: S#Tx)
    extends Stream[S, A] {

    private[this] val id        = tx0.newId()
    private[this] val _hasNext  = tx0.newBooleanVar(id, true)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private def simpleString = s"Stream.single($elem)"

    override def toString = s"$simpleString; hasNext = ${_hasNext}"

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

    def reset()(implicit tx: S#Tx): Unit =
      _hasNext() = true

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!_hasNext()) Stream.exhausted()
      _hasNext() = false
      logStream(s"$simpleString.next()")
      elem
    }
  }

  def apply[S <: Base[S], A](elems: A*)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    new Seq(elems, tx)

  private final class Seq[S <: Base[S], A](elems: scala.Seq[A], tx0: S#Tx)
    extends Stream[S, A] {

    private[this] val id    = tx0.newId()
    private[this] val count = tx0.newIntVar(id, 0)
    private[this] val xs    = elems.toIndexedSeq

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private[this] lazy val simpleString =
      elems.mkString("Stream(", ", ", ")")

    override def toString = s"$simpleString; count = $count"

    def reset()(implicit tx: S#Tx): Unit =
      count() = 0

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = count() < xs.size

    def next ()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val i = count()
      count() = i + 1
      val res = elems(i)
      // logStream(s"$simpleString.next(); count = $i; res = $res")
      res
    }
  }

  private final class FlatMap[S <: Base[S], A, B](outer: Stream[S, A], f: A => Stream[S, B], tx0: S#Tx)
                                       (implicit ctx: Context[S])
    extends Stream[S, B] {

    private[this] val id        = tx0.newId()
    private[this] val _valid    = tx0.newBooleanVar(id, false)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val sub       = tx0.newVar[Stream[S, B]](id, null)
    //    private[this] val _hasSub   = ctx.newVar(false)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      outer.reset()
      //        if (_hasSub()) sub().reset()
    }

    @tailrec
    private def step()(implicit tx: S#Tx): Unit = {
      val ohn = outer.hasNext
      if (ohn) {
        val _sub    = f(outer.next())
        sub()       = _sub
        //        _hasSub()   = true
        val shn     = _sub.hasNext
        _hasNext()  = shn
        if (!shn) step()
      } else {
        //        _hasSub()   = false
        _hasNext()  = false
      }
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        step()
      }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): B = {
      if (!hasNext) Stream.exhausted()
      val _sub = sub()
      val res = _sub.next()
      if (!_sub.hasNext) step()
      res
    }
  }

  private final class Map[S <: Base[S], A, B](outer: Stream[S, A], f: A => B) extends Stream[S, B] {
    def reset()(implicit tx: S#Tx): Unit = outer.reset()

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = outer.hasNext
    def next ()(implicit ctx: Context[S], tx: S#Tx): B       = f(outer.next())
  }

  private final class Zip[S <: Base[S], A, B](outer: Stream[S, A], that: Stream[S, B]) extends Stream[S, (A, B)] {

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = {
      outer.reset()
      that .reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = outer.hasNext && that.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): (A, B) = (outer.next(), that.next())
  }

  private final class ++ [S <: Base[S], A, B >: A](outer: Stream[S, A], that: Stream[S, B]) extends Stream[S, B] {

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = {
      outer.reset()
      that .reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = outer.hasNext || that.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): B =
      if (outer.hasNext) outer.next() else that.next()
  }

  private final class Take[S <: Base[S], A](outer: Stream[S, A], n: Int, tx0: S#Tx)
    extends Stream[S, A] {

    private[this] val id      = tx0.newId()
    private[this] val _count  = tx0.newIntVar(id, 0)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = outer.reset()

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _count() < n && outer.hasNext

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      _count() = _count() + 1
      outer.next()
    }
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

  final def map[B](f: A => B)(implicit ctx: Context[S], tx: S#Tx): Stream[S, B] =
    new Stream.Map(outer, f)

  final def flatMap[B](f: A => Stream[S, B])(implicit ctx: Context[S], tx: S#Tx): Stream[S, B] =
    new Stream.FlatMap(outer, f, tx)

  final def zip[B](that: Stream[S, B])(implicit ctx: Context[S], tx: S#Tx): Stream[S, (A, B)] =
    new Stream.Zip(outer, that)

  final def ++ [B >: A](that: Stream[S, B]): Stream[S, B] = new Stream.++(outer, that)

  final def take(n: Int)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new Stream.Take(outer, n, tx)

  final def drop(n: Int)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    var j = 0
    while (j < n && hasNext) {
      next()
      j += 1
    }
    this
  }

  final def isEmpty (implicit ctx: Context[S], tx: S#Tx): Boolean = !hasNext
  final def nonEmpty(implicit ctx: Context[S], tx: S#Tx): Boolean = hasNext

  final def toList(implicit ctx: Context[S], tx: S#Tx): List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  final def toVector(implicit ctx: Context[S], tx: S#Tx): Vector[A] = {
    val b = Vector.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  final def size(implicit ctx: Context[S], tx: S#Tx): Int = {
    var res = 0
    while (hasNext) {
      next()
      res += 1
    }
    res
  }

  final def foreach(fun: A => Unit)(implicit ctx: Context[S], tx: S#Tx): Unit =
    while (hasNext) fun(next())
}
