package de.sciss.patterns

import de.sciss.patterns.Types.TopT
import de.sciss.patterns.graph._

/**
  * Almat: work in progress, seeing how
  * `tuple working 31.10.scd` can be translated to Scala.
  */
object RonTuple {
  // some extra operations
  implicit class SeqOps[A](xs: Seq[A]) {
    // like Kollflitz' `differentiate` but keeps first value
    def differentiate(implicit num: Numeric[A]): Seq[A] = {
      import num._
      if (xs.isEmpty) Seq.empty
      else xs.head +: xs.sliding(2).map { case Seq(a, b) => b - a }.toList
    }

    def stutter(n: Int): Seq[A] = xs.flatMap(a => Seq.fill(n)(a))
  }

  // all pairs from two arrays
  def directProduct[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
    a.map { v => b.flatMap { w => v :+ w }}

  // collects every occurrence of elements of t in s
  def extract[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] =
    t.map { tj =>
      s.zipWithIndex.collect {
        case (b, i) if b == tj => i
      }
    }

  // generates all tuplets from within x, an array
  // where each element is an array of occurrences of a value
  def allTuples[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
    val size = x.size
    var current: Seq[Seq[A]] = x.head.map(Seq(_))
    for (i <- 1 until size) {
      current = directProduct(current, x(i))
    }
    current
  }

  // computes the duration of a set of time points relative toa cycle.
  def computeDur[A](tps: Seq[A], cycle: A)(implicit num: Integral[A]): A = {
    import num._
    val dur0  = tps.differentiate.tail
    val dur1  = dur0.map(_ % cycle)
    val dur   = dur1.map { v => if (v == 0) cycle else v }
    dur.sum
  }

  // function to sort a groups of time points based on their total duration
  def sortTuples[A](array: Seq[Seq[A]], cycle: A)(implicit num: Integral[A]): Seq[Seq[A]] = {
    import num._
    array.sortWith { (a, b) =>
      computeDur[A](a, cycle) <= computeDur[A](b, cycle)
    }
  }

  // computes and sorts all possible sub patterns of a pattern
  def computeDurs[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0): Seq[Int] = {
    val positions = extract(cantus, pattern)
    val tuples0   = allTuples(positions)
    val tuples    = tuples0.sortWith({ (a, b) => computeDur(a, 7) > computeDur(b, 7) })
    val clump     = (Seq(start % cantus.size) ++ tuples.flatten).sliding(2).toList
    val durs      = clump.map { case Seq(pr0, pr1) =>
        val dur0 = (pr1 - pr0) % cantus.size
        if (dur0 == 0) { cantus.size } else dur0
      }
    durs
  }

  def makePart[A, T <: TopT[A]](pattern: Seq[A], cantus: Seq[A], start: Int = 0, stutter: Int = 1)
                               (implicit view: A => Pat[T]): (Pat[T], Pat.Double) = {
    val durs1 = {
      val durs0 = computeDurs(pattern, cantus, start).map(_.toDouble)
      if (stutter == 1) durs0 else durs0.stutter(stutter).map(_ / stutter)
    }
    val durs  = durs1.map(_ * 0.02)
    val inf   = Int.MaxValue

//    val ptrnOut = if (stutter == 1) {
//      Seq('r, Pseq(pattern, inf))
//    } else {
//      Seq(Pseq(Seq.fill(stutter)('r)),
//        Pseq(pattern.grouped(stutter).stutter(stutter).flatten, inf))
//    }
    val ptrnOut = Seq(Pseq(pattern, inf))

//    Zip(Seq(Pseq(ptrnOut), Pseq(durs)))
    (Pseq(ptrnOut), Pseq(durs))
  }
}