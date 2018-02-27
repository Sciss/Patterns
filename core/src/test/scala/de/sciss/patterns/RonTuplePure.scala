package de.sciss.patterns

import de.sciss.patterns.Types.{DoubleTop, IntTop, Num}
import de.sciss.patterns.graph._

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
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    val x = Graph { mkGraph[Unit]() }
    val it = x.expand[Unit]
    println("Done.")
    var time = 0.0
//    showStreamLog = true
    val xs = it.take(1000).toList
    println(s"Size = ${xs.size}")
    xs.foreach { elem0: Event =>
      val elem  = elem0 +
        (Event.keyDetunedFreq -> Event.detunedFreq(elem0)) +
        (Event.keySustain     -> Event.sustain    (elem0)) +
        (Event.keyAmp         -> Event.amp        (elem0))

      val elemS = elem.map.mapValues(mkElemString).mkString("(", ", ", ")")
      println(f"t = $time%g: $elemS")
      //      println(f"t = $time%g: $elem")

      time += Event.delta(elem)
    }
    println(f"Stop time = $time%g")   // should last around 6 minutes
  }

  // some extra operations
  implicit class SeqOps[A](xs: Seq[A]) {
    // like Kollflitz' `differentiate`
    def differentiate(implicit num: Numeric[A]): Seq[A] = {
      import num._
      xs.sliding(2).map { case Seq(a, b) => b - a }.toList
    }

    def stutter(n: Int): Seq[A] = xs.flatMap(a => Seq.fill(n)(a))

    def mirror: Seq[A] = if (xs.isEmpty) xs else xs ++ xs.reverse.tail

    def choose[Tx]()(implicit r: Random[Tx], tx: Tx): A = xs(r.nextInt(xs.size))
  }

  // all pairs from two arrays
  def directProduct_Sq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] = {
    val res = a.flatMap { v =>
      b.map { w => v :+ w }
    }
    log("directProduct", a, b, " => ", res)
    res
  }

//  // all pairs from two arrays
//  def directProduct[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
//    a.map { v: Pat[A] => v ++ b }

  // all pairs from two arrays
  def directProduct[A](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
    a.flatMap { v: Pat[A] =>
      b.bubble.map { w: Pat[A] => v ++ w }
    }

  // collects the indices of every occurrence of elements of t in s
  def extract_Sq[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] = {
    val res = t.map { tj =>
      s.zipWithIndex.collect {
        case (b, i) if b == tj => i
      }
    }
    log("extract", s, t, " => ", res)
    res
  }

  // collects the indices of every occurrence of elements of t in s
  def extract[A](s: Pat[A], t: Pat[A]): Pat[Pat[Int]] =
    t.bubble.map { tj: Pat[A] =>
      val same      = s sig_== Hold(tj)
      val indices   = s.indices
      val indicesF  = Gate(indices, same)
      indicesF
    }

  // generates all tuplets from within x, an array
  // where each element is an array of occurrences of a value
  def allTuples_Sq[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
    val size = x.size
    var res: Seq[Seq[A]] = x.head.map(Seq(_))
    for (i <- 1 until size) {
      res = directProduct_Sq(res, x(i))
    }
    log("allTuples", x, " => ", res)
    res
  }

  def allTuples_Sq2[A](x: Seq[Seq[A]]): Seq[Seq[A]] = {
    val hd +: tl = x
    val res = tl.foldLeft(hd.map(Seq(_)))((ys, xi) => directProduct_Sq(ys, xi))
    log("allTuples", x, " => ", res)
    res
  }

  // generates all tuplets from within x, an array
  // where each element is an array of occurrences of a value
  def allTuples[A](x: Pat[Pat[A]]): Pat[Pat[A]] = {
    val hd = x.head.bubble
    val tl = x.tail
    tl.foldLeft(hd)((ys: Pat[Pat[A]], xi: Pat[A]) => directProduct(ys, xi))
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
  def computeDur_Sq[A](tps: Seq[A], cycle: A)(implicit num: Integral[A]): A = {
    import num._
    val dur0  = tps.differentiate
    //    val dur1  = dur0.map(_ % cycle)
    val dur1  = dur0.map(mod(_, cycle))
    val dur   = dur1.map { v => if (v == zero) cycle else v }
    val res   = dur.sum
    //    log("computeDur", tps, cycle, " => ", res)
    res
  }

  // computes the duration of a set of time points relative to a cycle.
  def computeDur[A](tps: Pat[A], cycle: Pat[A])(implicit num: Num[A]): Pat[A] = {
    val one  = Constant(num.one)
    val dur0 = tps.differentiate
    val dur  = ((dur0 - one) mod cycle) + one
    dur.sum
  }

  // function to sort groups of time points based on their total duration
  def sortTuples_Sq[A](array: Seq[Seq[A]], cycle: A)(implicit num: Integral[A]): Seq[Seq[A]] = {
    array.sortWith { (a, b) =>
      num.lteq(computeDur_Sq[A](a, cycle), computeDur_Sq[A](b, cycle))
    }
  }

  // computes and sorts all possible sub patterns of a pattern
  def computeDurs_Sq[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0): Seq[Int] = {
    val positions = extract_Sq(cantus, pattern)
    val tuples0   = allTuples_Sq(positions)
    //    val tuples    = tuples0.sortWith { (a, b) => computeDur(a, 7) > computeDur(b, 7) }
    val tuples    = tuples0.sortWith { (a, b) =>
      val ad = computeDur_Sq(a, 7)
      val bd = computeDur_Sq(b, 7)
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

  // computes and sorts all possible sub patterns of a pattern
  def computeDurs[A](pattern: Pat[A], cantus0: Pat[A], start: Pat[Int] = 0): Pat[Int] = {
    val cantus = cantus0

    val positions : Pat[Pat[Int]] = extract(cantus, pattern) // <| (_.size.poll("positions.size"))
    val tuples0   : Pat[Pat[Int]] = allTuples(positions)     // <| (_.size.poll("tuple0.size"))
    val tuples    : Pat[Pat[Int]] = tuples0.sortWith { (a, b) =>
      val ad = computeDur(a, 7)
      val bd = computeDur(b, 7)
//        // handle algorithmic ambiguity
//        if (ad == bd) {
//          (a zip b).collectFirst {
//            case (ai, bi) if ai < bi => true
//            case (ai, bi) if ai > bi => false
//          } .get // OrElse(true)
//        } else {
        ad > bd
//        }
    }
    val cantusSz = cantus.size   // .poll("cantusSz")
    val clump: Pat[Pat[Int]] = ((start mod cantusSz) ++ tuples.flatten).sliding(2)
    val durs      = clump.flatMap { pr: Pat[Int] =>
      val (pr0, pr1) = pr.splitAt(1)
      val dur0 = ((pr1 - pr0 - 1) mod cantusSz) + 1
      dur0: Pat[Int] // if (dur0 == 0) { cantus.size } else dur0
    }
    durs
  }

  def makePart_Sq[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0, stutter: Int = 1)
                    (implicit view: A => Pat[A]): (Pat[A], Pat[Double]) = {
    log("makePart", pattern, cantus, start, stutter)
    val durs = {
      val durs0 = computeDurs_Sq(pattern, cantus, start).map(_.toDouble)
      durs0 // if (stutter == 1) durs0 else durs0.stutter(stutter).map(_ / stutter)
    }
//    val inf   = Int.MaxValue

    //    val ptrnOut = if (stutter == 1) {
    //      Seq('r, Pseq(pattern, inf))
    //    } else {
    //      Seq(Pseq(Seq.fill(stutter)('r)),
    //        Pseq(pattern.grouped(stutter).stutter(stutter).flatten, inf))
    //    }
    val ptrnOut = Pat.loop()(pattern) // Pseq(pattern, inf)

    //    Zip(Seq(Pseq(ptrnOut), Pseq(durs)))
    log("makePart - durs", durs)
    (ptrnOut, durs.map(_ * 0.02))
  }

  def makePart[A](pattern: Pat[A], cantus: Pat[A], start: Pat[Int] = 0, stutter: Pat[Int] = 1): (Pat[A], Pat[Double]) = {
    val durs: Pat[Int] = {
      val durs0 = computeDurs(pattern, cantus, start) // <| (_.poll("computeDurs")) // .map(_.toDouble)
      durs0 // if (stutter == 1) durs0 else durs0.stutter(stutter).map(_ / stutter)
    }
//    val inf = Int.MaxValue

    //    val ptrnOut = if (stutter == 1) {
    //      Seq('r, Pseq(pattern, inf))
    //    } else {
    //      Seq(Pseq(Seq.fill(stutter)('r)),
    //        Pseq(pattern.grouped(stutter).stutter(stutter).flatten, inf))
    //    }
    val ptrnOut: Pat[A] = pattern.loop() // Pseq(List(pattern), inf)

    //    Zip(Seq(Pseq(ptrnOut), Pseq(durs)))
    // (Pseq(ptrnOut), Pseq(durs.map(_ * 0.02)))
    (ptrnOut, durs * 0.02)
  }

  def mkGraph[Tx](): Pat[Event] = {
//    val inf = Int.MaxValue

    def mkNotes(in: Pat[Int]): Pat[Double] = in * 2.4 + 4.0

    def catPat(cantus: Pat[Int]): Pat[Event] =
      Bind(
        "instrument"  -> "sine4",
        "note"        -> mkNotes(cantus),
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
    val lPat = Pat.loop()((8 to 12).mirror)
    val rPat = Pat.loop()((5 to  9).mirror.map(_/25.0))
//    val sPat = White(1, 4)

    val xs = for {
      len     <- lPat.bubble
      rests   <- rPat.bubble
//      stutter <- sPat.bubble
      cantus0 <- Brown(-6, 6, 3).grouped(len)
    } yield {
//      val cantus0 = Pat(13.6, 8.8, 6.4, 13.6, 18.4, 16.0, 18.4, 18.4)
//      val cantus0 = Pat(6.4, 8.8, 6.4, 8.8, 8.8, 13.6, 8.8, 13.6).take(length)
      val numPause  = (len * rests).toInt
      //      println(numPause)
      val cantus = cantus0 // Pat.fold(cantus0, numPause)(in => in.update(in.size.rand, 'r))
      if (DEBUG) println(s"starting ${mkElemString(cantus)}")
//      val cantusEvt = catPat(cantus)
      //      val catter = sp.par(cantusEvt)

      //      println(s"CANTUS $cantus")

      val pitchSets0: Pat[Pat[Int]] =
        cantus.distinct.sorted.combinations(3) // <| (_.size.poll("numParts"))

      val pitchSets = pitchSets0.map(_.shuffle)

//      if (DEBUG) println("PARTS:")
      // if (DEBUG) parts.foreach(p => println(mkElemString(p)))

      //      var durs = List.empty[Double]

      val numParts0 = pitchSets.size // .poll("numParts")
      val numParts = numParts0.hold()
      // XXX TODO: The following is wrong -- `flow()`
      // should only be effective inside `parts.map` but not for
      // `Pat.loop`
      val pats: Pat[Pat[Event]] = pitchSets.mapWithIndex { (part0, partsIdx0) =>
        val partsIdx  = partsIdx0  // .poll("partsIdx")
        val partsIdxH = partsIdx.hold() // <| (_.poll("partsIdx"))
//          val (notePat, durPat) = makePart(part, cantus, 0, Seq(1,1,2,2,4).choose())
        val part    = part0 // <| (_.poll("part"))
        val (notePat0, durPat0) = makePart(part, cantus) // , stutter = stutter)
        val notePat = notePat0 // <| (_.poll("notePat"))
        val durPat  = durPat0  // <| (_.poll("durPat"))
        val legato  = partsIdxH.linlin(0, numParts, 0.02, 1.0)
        val i       = partsIdxH
        val db      = partsIdxH.linlin(0, numParts, -40.0, -30.0)

        Bind(
//          "instrument"  -> "sine4",
          "note"        -> mkNotes(notePat),
          "dur"         -> durPat,
          "octave"      -> 5,
          "legato"      -> legato,
          "detune"      -> -2.0, // White(-2.0,2.0),
          "i"           -> i,
          "ar"          -> 0.001,
          "dr"          -> 0.1,
          "stretch"     -> 1,
          "db"          -> db
        )
      }

//      val patsF: Pat.Event = pats.flatten

      //      println(s"DURS IS ${mkElemString(durs)} - MAX ${mkElemString(durs.max)}")

//      val patsF: Pat[Event] = Ppar(pats)
//
//println("RonTuplePure: TODO")
//patsF

      pats.flatten // (0)

//      ... // sp.seq(patsF) // Ppar(patsF))
//      //      println(s"ending $cantus")
//      // at this point, it wouldn't have any effect:
//      // { cantus(cantus.size.rand) = 'r }.dup(5)
//      val stopTime = length * 2 * 0.2
//      ... // sp.advance(stopTime)
//      ... // sp.suspend(catter)
//      ...
    }
    xs.flatten
  }
}