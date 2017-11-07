package de.sciss.patterns

import scala.collection.AbstractIterator
import scala.language.implicitConversions

object Types {
  def main(args: Array[String]): Unit = example()

  trait Top {
    type Out
  }

  trait Bridge[T1 <: Top, T2 <: Top, T <: Top] {
    val tpe: T

    def lift1(a: T1#Out): tpe.Out
    def lift2(a: T2#Out): tpe.Out
  }

  trait Num[T1 <: Top, T2 <: Top, T <: Top] extends Bridge[T1, T2, T] {
    def plus (a: tpe.Out, b: tpe.Out): tpe.Out
    def times(a: tpe.Out, b: tpe.Out): tpe.Out
  }

  trait PE[T <: Top] {
    val tpe: T

    def iterator: Iterator[tpe.Out]
  }

  trait IntLikeNum {
    final val tpe: IntSeqTop = IntSeqTop

    final def plus (a: Seq[Int], b: Seq[Int]): Seq[Int] = combine(a, b)(_ + _)
    final def times(a: Seq[Int], b: Seq[Int]): Seq[Int] = combine(a, b)(_ * _)

    private def combine(a: Seq[Int], b: Seq[Int])(op: (Int, Int) => Int): Seq[Int] = {
      val as = a.size
      val bs = b.size
      val sz = math.max(as, bs)
      Seq.tabulate(sz) { i =>
        op(a(i % as), b(i % bs))
      }
    }
  }

  sealed trait IntLikeTop extends Top {
    def lift(in: Out): Seq[Int]
  }

  sealed trait IntSeqTop extends IntLikeTop {
    type Out = Seq[Int]
  }
  implicit object IntSeqTop extends IntSeqTop with IntLikeNum with Num[IntSeqTop, IntSeqTop, IntSeqTop] {
    def lift(in: Seq[Int]): Seq[Int] = in

    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  sealed trait IntTop extends IntLikeTop {
    override type Out = Int
  }
  implicit object IntTop extends IntTop with Num[IntTop, IntTop, IntTop] {
    val tpe: IntTop = this

    def lift(in: Int): Seq[Int] = in :: Nil

    def lift1(a: Int): Int = a
    def lift2(a: Int): Int = a

    def plus (a: Int, b: Int): Int = a + b
    def times(a: Int, b: Int): Int = a * b
  }

//  sealed trait DoubleSeqTop extends NumTop /* DoubleLikeTop */ {
//    def bridge(a: NumTop, b: NumTop): Bridge[a.Out, b.Out, Out] = ...
//  }
//  implicit object DoubleSeqTop extends DoubleSeqTop
//
//  sealed trait DoubleTop extends DoubleSeqTop


  implicit object intSeqNum1 extends IntLikeNum with Num[IntTop, IntSeqTop, IntSeqTop] {
    def lift1(a: Int     ): Seq[Int] = a :: Nil
    def lift2(a: Seq[Int]): Seq[Int] = a
  }

  implicit object intSeqNum2 extends IntLikeNum with Num[IntSeqTop, IntTop, IntSeqTop] {
    def lift1(a: Seq[Int]): Seq[Int] = a
    def lift2(a: Int     ): Seq[Int] = a :: Nil
  }

  ////////////////////////////

  final case class Add[T1 <: Top, T2 <: Top, T <: Top](a: PE[T1], b: PE[T2])
                                                      (implicit val br: Num[T1, T2, T])
    extends PE[T] {

    val tpe: br.tpe.type = br.tpe

    def iterator: Iterator[tpe.Out] = {
      val ai = a.iterator
      val bi = b.iterator

      new AbstractIterator[tpe.Out] {
        def hasNext: Boolean = ai.hasNext && bi.hasNext

        def next(): tpe.Out = {
          val an  = br.lift1(ai.next())
          val bn  = br.lift2(bi.next())
          val res = br.plus(an, bn)
          res
        }
      }
    }
  }

  trait SeriesLike[T1 <: Top, T2 <: Top, T <: Top] extends PE[T] {
    // ---- abstract ----

    def start: PE[T1]

    protected val br: Bridge[T1, T2, T]

    protected def step: PE[T2]

    protected def op(a: tpe.Out, b: tpe.Out): tpe.Out

    // ---- impl ----

    final val tpe: br.tpe.type = br.tpe

    final def iterator: Iterator[tpe.Out] = {
      val ai = start.iterator.map(br.lift1)
      val bi = step .iterator.map(br.lift2)

      if (ai.isEmpty) Iterator.empty
      else new AbstractIterator[tpe.Out] {
        private var state: tpe.Out = ai.next
        var hasNext = true

        def next(): tpe.Out = {
          val res = state
          hasNext = ai.hasNext && bi.hasNext
          if (hasNext) {
            state = op(state, bi.next())
          }
          res
        }
      }
    }
  }

  final case class Series[T1 <: Top, T2 <: Top, T <: Top](start: PE[T1], step: PE[T2])
                                                         (implicit val br: Num[T1, T2, T])
    extends SeriesLike[T1, T2, T] {

    protected def op(a: tpe.Out, b: tpe.Out): tpe.Out = br.plus(a, b)
  }

  final case class Geom[T1 <: Top, T2 <: Top, T <: Top](start: PE[T1], step: PE[T2])
                                                         (implicit val br: Num[T1, T2, T])
    extends SeriesLike[T1, T2, T] {

    protected def op(a: tpe.Out, b: tpe.Out): tpe.Out = br.plus(a, b)
  }

  final case class Cat[T1 <: Top, T2 <: Top, T <: Top](a: PE[T1], b: PE[T2])
                                                      (implicit val br: Bridge[T1, T2, T]) extends PE[T] {
    val tpe: br.tpe.type = br.tpe

    def iterator: Iterator[tpe.Out] = {
      val ai = a.iterator.map(br.lift1)
      val bi = b.iterator.map(br.lift2)
      ai ++ bi
    }
  }

  final case class Take[T <: Top](a: PE[T], length: PE[IntTop])
    extends PE[T] {

    val tpe: a.tpe.type = a.tpe

    def iterator: Iterator[tpe.Out] = {
      val lenIt = length.iterator
      if (lenIt.isEmpty) Iterator.empty
      else {
        val lenVal = lenIt.next()
        a.iterator.take(lenVal)
      }
    }
  }

  sealed trait StringTop extends Top {
    type Out = String
  }
  implicit object StringTop extends StringTop with Bridge[StringTop, StringTop, StringTop] {
    val tpe: StringTop = this

    def lift1(a: String): String = a
    def lift2(a: String): String = a
  }

  final case class Const[A, T <: Top](x: A)(implicit val tpe: T { type Out = A }) extends PE[T] {
    def iterator: Iterator[A] = Iterator.continually(x)
  }

  implicit class ElemOps[T <: Top](private val x: PE[T]) extends AnyVal {
    def take(length: PE[IntTop]): Take[T] = Take(x, length)
  }

//  implicit def const[A, T <: Top](x: A)(implicit tpe: T { type Out = A }): Elem[T] = Const(x)

  implicit def intElem      (x: Int         ): PE[IntTop      ] = Const(x)
  implicit def intSeqElem   (xs: Seq[Int]   ): PE[IntSeqTop   ] = Const(xs)
//  implicit def doubleElem   (i: Double      ): Elem[DoubleTop   ] = ...
//  implicit def doubleSeqElem(i: Seq[Double] ): Elem[DoubleSeqTop] = ...
  implicit def stringElem   (x: String      ): PE[StringTop   ] = Const(x)

  def example(): Unit = {
    // ok
    val a = Add[IntSeqTop, IntTop, IntSeqTop](Seq(1, 2), 3)
    println(a.iterator.take(1).mkString("a: ", ", ", ""))

    // ok
    val b = Add(Seq(1, 2), 3)
    println(b.iterator.take(1).mkString("b: ", ", ", ""))

    // ok
    val c = Add(1, 3)
    c.iterator: Iterator[Int] // right
    println(c.iterator.take(1).mkString("c: ", ", ", ""))

    // ok
    val d = Add(1, Seq(2, 3))
    println(d.iterator.take(1).mkString("d: ", ", ", ""))

    // ok
    val e = Add(Seq(1, 2), Seq(3, 4))
    println(e.iterator.take(1).mkString("e: ", ", ", ""))

    // ok
    val f = Series(Seq(2, 3), 4)
    println(f.iterator.take(3).mkString("f: ", ", ", ""))

    // ok
    val g = Cat(Take("foo", 2), "bar").take(5)
    g.iterator: Iterator[String]  // right
    println(g.iterator.take(7).mkString("g: ", ", ", ""))

    //    // ambiguous implicits
//    Foo(Seq(1, 2), 3.0)

    //    // compiles not
//    Foo(Seq(1, 2), "foo")
  }
}
