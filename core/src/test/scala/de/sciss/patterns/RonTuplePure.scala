package de.sciss.patterns

import de.sciss.patterns.Types.{DoubleTop, IntTop, Top, TopT}
import de.sciss.patterns.graph._

import scala.util.Random

object RonTuplePure {
  def mkElemString(in: Any): String = in match {
    case ch: Seq[_] => ch.map(mkElemString).mkString("[ ", ", ", " ]")
    case d: Double => val s = d.toFloat.toString; if (s.endsWith(".0")) s.dropRight(2) else s
    case other => other.toString
  }

  final val DEBUG = false
  final val LOG   = false

  def log(elems: Any*): Unit = {
    val s = mkElemString(elems)
    if (LOG) println(s)
  }

  def main(args: Array[String]): Unit = {
    val x = spawner()
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    val it = x.expand
    println("Done.")
    var time = 0.0
    it.foreach { elem0: Event#Out =>
      val elem  = elem0 +
        (Event.keyDetunedFreq -> Event.detunedFreq(elem0)) +
        (Event.keySustain     -> Event.sustain    (elem0)) +
        (Event.keyAmp         -> Event.amp        (elem0))

      val elemS = elem.mapValues(mkElemString).mkString("(", ", ", ")")
      println(f"t = $time%g: $elemS")
      //      println(f"t = $time%g: $elem")

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
  def directProduct_Sq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] = {
    val res = a.flatMap { v => b.map { w => v :+ w } }
    log("directProduct", a, b, " => ", res)
    res
  }

  // all pairs from two arrays
  def directProduct[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] = {
    ???
//    val res = a.flatMap { v => b.map { w => v :+ w } }
//    log("directProduct", a, b, " => ", res)
//    res
  }

  // collects the indices of every occurrence of elements of t in s
  def extract[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] = {
    val res = t.map { tj =>
      s.zipWithIndex.collect {
        case (b, i) if b == tj => i
      }
    }
    log("extract", s, t, " => ", res)
    res
  }

  // generates all tuplets from within x, an array
  // where each element is an array of occurrences of a value
  def allTuples[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
    val size = x.size
    var res: Seq[Seq[A]] = x.head.map(Seq(_))
    for (i <- 1 until size) {
      res = directProduct_Sq(res, x(i))
    }
    log("allTuples", x, " => ", res)
    res
  }

  // N.B. SuperCollider `mod` is different from `%` for negative numbers!
  def mod[A](a: A, b: A)(implicit num: Integral[A]): A = {
    import num._
    if (gteq(a, zero)) a % b else {
      val c = -a % b
      if (c == zero) zero else b - c
    }
  }

  // computes the duration of a set of time points relative to a cycle.
  def computeDur[A](tps: Seq[A], cycle: A)(implicit num: Integral[A]): A = {
    import num._
    val dur0  = tps.differentiate.tail
    //    val dur1  = dur0.map(_ % cycle)
    val dur1  = dur0.map(mod(_, cycle))
    val dur   = dur1.map { v => if (v == zero) cycle else v }
    val res   = dur.sum
    //    log("computeDur", tps, cycle, " => ", res)
    res
  }

  // function to sort groups of time points based on their total duration
  def sortTuples[A](array: Seq[Seq[A]], cycle: A)(implicit num: Integral[A]): Seq[Seq[A]] = {
    array.sortWith { (a, b) =>
      num.lteq(computeDur[A](a, cycle), computeDur[A](b, cycle))
    }
  }

  // computes and sorts all possible sub patterns of a pattern
  def computeDurs[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0): Seq[Int] = {
    val positions = extract(cantus, pattern)
    val tuples0   = allTuples(positions)
    //    val tuples    = tuples0.sortWith { (a, b) => computeDur(a, 7) > computeDur(b, 7) }
    val tuples    = tuples0.sortWith { (a, b) =>
      val ad = computeDur(a, 7)
      val bd = computeDur(b, 7)
      // handle algorithmic ambiguity
      if (ad == bd) {
        (a zip b).collectFirst {
          case (ai, bi) if ai < bi => true
          case (ai, bi) if ai > bi => false
        } .get // OrElse(true)
      } else {
        ad > bd
      }
    }
    log("computeDurs -- tuples", tuples)
    val clump     = (Seq(mod(start, cantus.size)) ++ tuples.flatten).sliding(2).toList
    val durs      = clump.map { case Seq(pr0, pr1) =>
      val dur0 = mod(pr1 - pr0, cantus.size)
      if (dur0 == 0) { cantus.size } else dur0
    }
    log("computeDurs", pattern, cantus, start, " => ", durs)
    durs
  }

  def makePart[A, T <: TopT[A]](pattern: Seq[A], cantus: Seq[A], start: Int = 0, stutter: Int = 1)
                               (implicit view: A => Pat[T]): (Pat[T], Pat.Double) = {
    log("makePart", pattern, cantus, start, stutter)
    val durs = {
      val durs0 = computeDurs(pattern, cantus, start).map(_.toDouble)
      durs0 // if (stutter == 1) durs0 else durs0.stutter(stutter).map(_ / stutter)
    }
    val inf   = Int.MaxValue

    //    val ptrnOut = if (stutter == 1) {
    //      Seq('r, Pseq(pattern, inf))
    //    } else {
    //      Seq(Pseq(Seq.fill(stutter)('r)),
    //        Pseq(pattern.grouped(stutter).stutter(stutter).flatten, inf))
    //    }
    val ptrnOut: Seq[Pseq[T]] = Seq(Pseq(pattern, inf))

    //    Zip(Seq(Pseq(ptrnOut), Pseq(durs)))
    log("makePart - durs", durs)
    (Pseq(ptrnOut), Pseq(durs.map(_ * 0.02)))
  }

  def spawner(): Pat.Event = {
    implicit val random: Random = new Random()
    val inf = Int.MaxValue
    def catPat(cantus: Pat.Double): Pat.Event =
      Bind(
        "instrument"  -> "sine4",
        "note"        -> cantus, // Pseq(cantus, inf), // Prout({ loop{ Pseq(~cantus).embedInStream } }),
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
    val lPat  = Pseq[IntTop   ]((8 to 12).mirror            , inf) // .iterator
    val rPat  = Pseq[DoubleTop]((5 to  9).mirror.map(_/25.0), inf) // .iterator
    //    lPat.next(); rPat.next()
    Pat.seqFill(4) { _ => // original: infinite
      // XXX TODO: ~tupletempo.tempo = ((10..20)/30).choose /2;
      val length    = lPat // .next()
      val cantus0: Pat.Double = ((Brown(-6, 6, 3): Pat.Int) * 2.4 + 4.0).take(length) // .iterator.take(length).toList
      val numPause  = (length * rPat /* .next() */).roundTo(1.0) // .toInt
      //      println(numPause)
      val cantus = cantus0 // (cantus0 /: (1 to numPause))((in, _) => in) // in.update(in.size.rand) = 'r)
      if (DEBUG) println(s"starting ${mkElemString(cantus)}")
      val cantusEvt = catPat(cantus)
      //      val catter = sp.par(cantusEvt)

      //      println(s"CANTUS $cantus")

      val parts: Pat[Pat.Double] = cantus.distinct  .sorted /* ! */ .combinations(3) // .toList

      if (DEBUG) println("PARTS:")
      // if (DEBUG) parts.foreach(p => println(mkElemString(p)))

      //      var durs = List.empty[Double]

      // val numParts = parts.size
      val partsIdx = Indices(parts)
      val pats: Pat[Pat.Event] = parts.map { part: Pat.Double =>
//          val (notePat, durPat) = makePart(part, cantus, 0, Seq(1,1,2,2,4).choose())
        val notePat: Pat.Double = ???
        val durPat : Pat.Double = ???

          //        durs ::= durPat.iterator.sum

        Bind(
          "instrument"  -> "sine4",
          "note"        -> notePat,
          "dur"         -> durPat,
          //        Pfunc({ ("voice" + i + "done").postln; nil })]),
          //          "db"          -> -15,
          "octave"      -> 5,
          "legato"      -> partsIdx.linlin(0, parts.size, 0.02, 1.0),
          "detune"      -> White(-2.0,2.0),
          //		out: Pseq((0..23), inf, i),
          "i"           -> Pseq(0 to 23, inf, partsIdx),
          "ar"          -> 0.001,
          "dr"          -> 0.1,
          "stretch"     -> 1,
          "db"          -> partsIdx.linlin(0, parts.size, -40.0, -30.0)
        )
      }

//      val patsF: Pat.Event = pats.flatten

      //      println(s"DURS IS ${mkElemString(durs)} - MAX ${mkElemString(durs.max)}")

      val patsF: Pat.Event = Ppar(pats)

      ??? // sp.seq(patsF) // Ppar(patsF))
      //      println(s"ending $cantus")
      // at this point, it wouldn't have any effect:
      // { cantus(cantus.size.rand) = 'r }.dup(5)
      val stopTime = length * 2 * 0.2
      ??? // sp.advance(stopTime)
      ??? // sp.suspend(catter)
      ???
    }
  }
}