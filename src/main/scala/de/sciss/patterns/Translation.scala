package de.sciss.patterns

import java.lang.reflect.InvocationTargetException

import org.coroutines.Coroutine

import scala.util.Random

object Translation {
//  // cf. https://alexn.org/blog/2017/01/16/iterator.html
//  trait Iterator[Tx, +A] {
//    def moveNext()(implicit tx: Tx): Boolean
//
//    def current(implicit tx: Tx): A
//
//    // def reset(): Unit
//  }
//
//  trait Pattern[Tx, +A] {
//    def iterator: Iterator[Tx, A]
//  }

  // ---- definitions ----

  object Pattern {

  }
  trait Pattern[+A] /* extends IIterable[A] */ { outer =>
    def iterator: Iterator[A]

    final def asStream: Iterator[A] = iterator

    final def map[B](f: A => B): Pattern[B] = new Pattern[B] {
      def iterator: Iterator[B] = outer.iterator.map(f)
    }

    final def filter(p: A => Boolean): Pattern[A] = new Pattern[A] {
      def iterator: Iterator[A] = outer.iterator.filter(p)
    }

    final def filterNot(p: A => Boolean): Pattern[A] = new Pattern[A] {
      def iterator: Iterator[A] = outer.iterator.filterNot(p)
    }
  }

  object Pfunc {
    def apply[A](next: => A): Pfunc[A] = new Pfunc[A](() => next)
  }
  final class Pfunc[A] private(next: () => A) extends Pattern[A] {
    def iterator: Iterator[A] = Iterator.continually(next())
  }

  object Prout {
    def apply[A](co: Coroutine._0[A, _]): Prout[A] = new Prout[A](co)
  }
  final class Prout[A] private(co: Coroutine._0[A, _]) extends Pattern[A] {
    def iterator: Iterator[A] = {
      import org.coroutines._
      new CoroutineIterator[A](call(co()))
    }
  }

  object Pseries {
    def apply[A: Numeric](start: A, step: A, length: Int = Int.MaxValue): Pseries[A] =
      new Pseries[A](start, step, length)
  }
  final class Pseries[A] private(start: A, step: A, length: Int)(implicit num: Numeric[A]) extends Pattern[A] {
    def iterator: Iterator[A] = {
      import num._
      Iterator.iterate(start)(_ + step).take(length)
    }
  }

  object Pgeom {
    def apply[A: Numeric](start: A, grow: A, length: Int = Int.MaxValue): Pgeom[A] =
      new Pgeom[A](start, grow, length)
  }
  final class Pgeom[A] private(start: A, grow: A, length: Int)(implicit num: Numeric[A]) extends Pattern[A] {
    def iterator: Iterator[A] = {
      import num._
      Iterator.iterate(start)(_ * grow).take(length)
    }
  }

  // ---- runner ----

  // run with example name as single argument, e.g. `Pfunc`
  def main(args: Array[String]): Unit = {
    val fName = args.headOption.getOrElse("Pfunc").capitalize
    val mName = s"ex$fName"
    try {
      val m = Translation.getClass.getMethod(mName)
      m.invoke(Translation)
    } catch {
      case _: NoSuchMethodException => println(s"No example for '$fName'")
      case e: InvocationTargetException =>
        throw e.getTargetException
    }
  }

  // ---- examples ----

  implicit val rnd: Random = new Random
  import SuperColliderOps._
  import de.sciss.kollflitz.RandomOps._

  implicit final class StreamOps(private val s: Iterator[Any]) extends AnyVal {
    def nextPrintln(): Unit = println(if (s.hasNext) s.next() else "nil")
  }

  def exPfunc(): Unit = {
    val a = Pfunc(Vector(1, 2, 3, 4).choose)
    val b = a.asStream                    // make a stream from the pattern
    5.iterate(_ => println(b.next))  // print 5 values from the stream
  }

  def exProut(): Unit = {
    import org.coroutines._
    val a = Prout(coroutine { () => yieldval(3.rand); yieldval(3.rand); yieldval(3.rand) })
    val b = a.asStream
    val c = a.asStream
    4.iterate(_ => b.nextPrintln())
    4.iterate(_ => c.nextPrintln())
  }

  def exPseries(): Unit = {
    val a = Pseries(10, 3, 8)
    val b = a.asStream
    9.iterate(_ => b.nextPrintln())
  }

  def exPgeom(): Unit = {
    val a = Pgeom(10, 3, 8)
    val b = a.asStream
    9.iterate(_ => b.nextPrintln())
  }

  // in SuperCollider: `collect`
  def exMap(): Unit = {
    val a = Pseries(0, 1, 10)
    val b = a.map(item => if (item.isEven) item + 100 else item)
    val c = b.asStream
    6.iterate(_ => c.nextPrintln())
  }

  // in SuperCollider: `select`
  def exFilter(): Unit = {
    val a = Pseries(0, 1, 10)
    val b = a.filter(item => item.isOdd)
    val c = b.asStream
    6.iterate(_ => c.nextPrintln())
  }

  // in SuperCollider: `reject`
  def exFilterNot(): Unit = {
    val a = Pseries(0, 1, 10)
    val b = a.filterNot(item => item.isOdd)
    val c = b.asStream
    6.iterate(_ => c.nextPrintln())
  }
}