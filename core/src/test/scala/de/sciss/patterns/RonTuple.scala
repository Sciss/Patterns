package de.sciss.patterns

import de.sciss.patterns.Types.{DoubleTop, IntTop, TopT}
import de.sciss.patterns.graph._
import de.sciss.numbers.Implicits._

import scala.util.Random

/**
  * Almat: work in progress, seeing how
  * `tuple working 31.10.scd` can be translated to Scala.
  */
object RonTuple {
  def main(args: Array[String]): Unit = {
    val x = spawner()
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    val it = x.expand
    println("Done.")
    var time = 0.0
    it.foreach { elem: Event#Out =>
//      val elemS = elem.map {
//        case (k, d: Double) => f"$k -> $d%g"
//        case (k, a)         => s"$k -> $a"
//      } .mkString("(", ", ", ")")
//      println(f"t = $time%g: $elemS")
      println(f"t = $time%g: $elem")

      time += Event.delta(elem)
    }
    println(f"Stop time = $time%g")   // should last around 6 minutes
  }

  // some extra operations
  implicit class SeqOps[A](xs: Seq[A]) {
    // like Kollflitz' `differentiate` but keeps first value
    def differentiate(implicit num: Numeric[A]): Seq[A] = {
      import num._
      if (xs.isEmpty) Seq.empty
      else xs.head +: xs.sliding(2).map { case Seq(a, b) => b - a }.toList
    }

    def stutter(n: Int): Seq[A] = xs.flatMap(a => Seq.fill(n)(a))

    def mirror: Seq[A] = if (xs.isEmpty) xs else xs ++ xs.reverse.tail

    def choose()(implicit r: Random): A = xs(r.nextInt(xs.size))
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
    array.sortWith { (a, b) =>
      num.lteq(computeDur[A](a, cycle), computeDur[A](b, cycle))
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
    val ptrnOut: Seq[Pseq[T]] = Seq(Pseq(pattern, inf))

//    Zip(Seq(Pseq(ptrnOut), Pseq(durs)))
    (Pseq(ptrnOut), Pseq(durs))
  }

  def spawner(): Pat.Event = Spawner { sp =>
    import sp.{context, random}
    val inf = Int.MaxValue
    def catPat(cantus: Seq[Double]): Pat.Event =
      Bind(
        "instrument"  -> "sine4",
        "note"        -> Pseq(cantus, inf), // Prout({ loop{ Pseq(~cantus).embedInStream } }),
        "dur"         -> 0.2,
        "db"          -> -45,
        "octave"      -> 5,
        "detune"      -> White(-2.0,2.0),
        "pan"         -> 0,
        "out"         -> White(0, 23),
        "i"           -> 4,
        "ar"          -> 0.001,
        "dr"          -> 0.1,
        "stretch"     -> 1
      )
    val lPat  = Pseq[IntTop   ]((8 to 12).mirror            , inf).iterator
    val rPat  = Pseq[DoubleTop]((5 to  9).mirror.map(_/25.0), inf).iterator
    for (_ <- 1 to 4) { // original: infinite
      // XXX TODO: ~tupletempo.tempo = ((10..20)/30).choose /2;
      val length    =  ??? : Int // lPat.next()
      val cantus0: Seq[Double] = ??? // ((Brown(-6, 6, 3): Pat.Int) * 2.4).iterator.take(length).toList.map(_ + 4)
      val numPause: Int = ??? // (length * rPat.next()).toInt
      println(numPause)
      val cantus = (cantus0 /: (1 to numPause))((in, _) => in) // in.update(in.size.rand) = 'r)
      println(s"starting $cantus")
      val catter: sp.Ref = ??? // sp.par(catPat(cantus))

      val parts = cantus.distinct.combinations(3).toList
      val pats = parts.zipWithIndex.map { case (part, i) =>
        val (notePat, durPat) = makePart(part, cantus, 0, Seq(1,1,2,2,4).choose())

        Bind(
          "instrument"  -> "sine4",
          "note"        -> notePat,
          "dur"         -> durPat,
//        Pfunc({ ("voice" + i + "done").postln; nil })]),
          "db"          -> -15,
          "octave"      -> 5,
          "legato"      -> i.linlin(0, parts.size, 0.02, 1),
          "detune"      -> White(-2.0,2.0),
        //		out: Pseq((0..23), inf, i),
          "i"           -> Pseq(0 to 23, inf, i),
          "ar"          -> 0.001,
          "dr"          -> 0.1,
          "stretch"     -> 1,
          "db"          -> i.linlin(0, parts.size, -40.0, -30.0)
        )
      }
      ??? // sp.seq(Ppar(pats))
      println(s"ending $cantus")
      // at this point, it wouldn't have any effect:
      // { cantus(cantus.size.rand) = 'r }.dup(5)
      val stopTime = length * 2 * 0.2
      println(f"--- stopTime = $stopTime%g")
      ??? // sp.advance(stopTime)
      ??? // sp.suspend(catter)
    }
  }
}