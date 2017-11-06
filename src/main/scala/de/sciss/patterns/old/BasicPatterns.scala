package de.sciss.patterns.old

import scala.collection.{Iterator => It}
import scala.util.Try

object BasicPatterns {
  def main(args: Array[String]): Unit = {
    val num  = args.headOption.flatMap(s => Try(s.toInt).toOption).getOrElse(1)
    println(s"Running ex$num()")
    val it = num match {
      case  1 => ex1()
      case  2 => ex2()
      case  3 => ex3()
      case  4 => ex4()
      case  5 => ex5()
      case  6 => ex6()
      case  7 => ex7()
      case  8 => ex8()
      case  9 => ex9()
      case 10 => ex10()
    }
    val res = it.toList
    println(s"Result: $res")
  }

  type Res = It[Int]

  /*

  ~stream = 3;
  10.collect { | i | ~stream.next(i * 100) }
  [ 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 ]

   */
  def ex1(): Res = {
    val stream = It.continually(3)
    It.fill(10)(stream.next)
  }

  /*

  r = Routine ({ | in |  loop { in = yield(in) } });
  10.collect { | i | r.next(i * 100) }
  [ 0, 100, 200, 300, 400, 500, 600, 700, 800, 900 ]

   */
  def ex2(): Res = {
    val r: Int => Int = identity
    It.tabulate(10)(r)
  }

  /*

  r = Routine ({ | in |  (1..10).do { | i | in = yield(i + in) } });
  10.collect { | i | r.next(i * 100) }
  [ 1, 102, 203, 304, 405, 506, 607, 708, 809, 910 ]

   */
  def ex3(): Res = {
    val r = It.range(1, 11)
    val q = It.range(1, 11).map(_ * 100)
    (r zip q).map { case (a, b) => a + b }
  }

  /*

  p = Routine({ (1..10).do{ | i | i.yield } });
  q = Routine({ (1..10).reverse.do{ | i | (i * 10).yield } });
  (p + q).nextN(11)
  [ 101, 92, 83, 74, 65, 56, 47, 38, 29, 20, nil ]

   */
  def ex4(): Res = {
    val r = It.range(1, 11)
    val q = It.range(10, 0, -1).map(_ * 10)
    (r zip q).map { case (a, b) => a + b } .take(10)
  }

  /*

  p = Routine({ (1..3).do{ | i | i.yield } });
  q = Routine({ (4..1).do{ | i | i.yield } });
  r = Routine({ p.embedInStream; q.embedInStream }).nextN(8);
  [ 1, 2, 3, 4, 3, 2, 1, nil ]

   */
  def ex5(): Res = {
    val p = It.range(1, 4)
    val q = It.range(4, 0, -1)
    val r = (p ++ q).take(8)
    r
  }

  /*

  p = Routine({ (1..3).do{ | i | i.yield } });
  q = Routine({ (4..1).do{ | i | i.yield } });
  r = Routine({ [p, 100, 500, q, 3, 2, 1].do { | v | v.embedInStream } }).nextN(13);
  [ 1, 2, 3, 100, 500, 4, 3, 2, 1, 3, 2, 1, nil ]

   */
  def ex6(): Res = {
    val p = It.range(1, 4)
    val q = It.range(4, 0, -1)
    val r = p ++ It.single(100) ++ It.single(500) ++ q ++ It.single(3) ++ It.single(2) ++ It.single(1)
    r.take(13)
  }

  /*

  p = Routine({ (1..3).do{ | i | i.yield } });
  r = p + p;
	r.nextN(4);
	[ 3, nil, nil, nil ]

  NOTE: this behaves differently in Scala -- it will throw an exception as the iterator is exhausted

   */
  def ex7(): Res = {
    val p = It.range(1, 4)
    val r = (p zip p).map { case (a, b) => a + b }
    r.take(4)
  }

  // we define pattern as a sort of iterator of iterator

  object Pattern {
    def apply[A](it: => It[A]): Pattern[A] =
      new Pattern[A] {
        def asStream: It[A] = it
      }
  }

  trait Pattern[A] { outer =>
    def asStream: It[A]

    def map[B](fun: A => B): Pattern[B] = new Pattern[B] {
      def asStream: It[B] = outer.asStream.map(fun)
    }

    def zip[B](that: Pattern[B]): Pattern[(A, B)] = new Pattern[(A, B)] {
      def asStream: It[(A, B)] = outer.asStream zip that.asStream
    }

    def ++[B >: A](that: Pattern[B]): Pattern[B] = new Pattern[B] {
      def asStream: It[B] = outer.asStream ++ that.asStream
    }
  }

  /*

  p = Pseq((1..3));  // a pattern that specifies a stream of 10 elements
  p.asStream.nextN(4)
  [ 1, 2, 3,  nil ]

   */
  def ex8(): Res = {
    val p = Pattern(It.range(1, 4))
    p.asStream.take(4)
  }

  /*

  r = p + p;
  r.asStream.nextN(4)
  [ 2, 4, 6, nil ]

   */
  def ex9(): Res = {
    val p = Pattern(It.range(1, 4))
    val r = (p zip p).map { case (a, b) => a + b }
    r.asStream.take(4)
  }

  /*

  q = Pseq([p,p]);
  q.asStream.nextN(7);
  [ 1, 2, 3, 1, 2, 3, nil ]

   */
  def ex10(): Res = {
    val p = Pattern(It.range(1, 4))
    val q = p ++ p
    q.asStream.take(7)
  }
}