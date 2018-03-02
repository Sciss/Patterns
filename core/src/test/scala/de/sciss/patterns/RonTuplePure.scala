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
    run(0)
  }

  def run(n: Int): Unit = {
    implicit val ctx: Context.Plain = Context()
    import ctx.tx
    ctx.setRandomSeed(0L)
    val t0 = System.currentTimeMillis()
    val x = Graph { mkGraph[Unit]() }
//    println(s"NUM PATS    = ${Pat   .COUNT}")

    val t1 = System.currentTimeMillis()
    val it = x.expand[Unit]
    val t2 = System.currentTimeMillis()
    println("Done.")
    var time = 0.0
//    showStreamLog = true
//    it.take(4000).foreach(_ => ())
//    val xs = Nil
    val xs = it.take(400).toList
    val t3 = System.currentTimeMillis()
    println(s"Size = ${xs.size}")
    println(s"Graph {} took ${t1-t0}ms")
    println(s"expand   took ${t2-t1}ms")
    println(s"toList   took ${t3-t2}ms")
//    println(s"NUM STREAMS = ${Stream.COUNT}")
//    println(s"NUM PATS    = ${Pat   .COUNT}")
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

    if (n > 0) run(n - 1)
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

  // N.B. SuperCollider `mod` is different from `%` for negative numbers!
  def mod[A](a: A, b: A)(implicit num: Integral[A]): A = {
    import num._
    if (gteq(a, zero)) a % b else {
      val c = -a % b
      if (c == zero) zero else b - c
    }
  }

  // all pairs from two arrays
  def directProduct_Sq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] = {
    val res = a.flatMap { v =>
      b.map { w => v :+ w }
    }
    log("directProduct", a, b, " => ", res)
    res
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

  def makePart_Sq[A](pattern: Seq[A], cantus: Seq[A], start: Int = 0, stutter: Int = 1): (Pat[A], Pat[Double]) = {
    log("makePart", pattern, cantus, start, stutter)
    val durs = {
      val durs0 = computeDurs_Sq(pattern, cantus, start).map(_.toDouble)
      val durs1 = durs0.grouped(stutter).toList.stutter(stutter).flatten[Double]
      durs1.map(_ / stutter)
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

  // ---------------------------------------------------------------------------------
  // purely pattern based version of the piece's note production
  // ---------------------------------------------------------------------------------

  // all pairs from two arrays
  def directProduct[A](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
    a.flatMap { v =>
      b.bubble.map { w => v ++ w }
    }

  // collects the indices of every occurrence of elements of t in s
  def extract[A](s: Pat[A], t: Pat[A]): Pat[Pat[Int]] =
    t.bubble.map { tj =>
      val same      = s sig_== tj.hold()
      val indices   = s.indices
      val indicesF  = Gate(indices, same)
      indicesF
    }

  // generates all tuplets from within x, an array
  // where each element is an array of occurrences of a value
  def allTuples[A](x: Pat[Pat[A]]): Pat[Pat[A]] = {
    val hd = x.head.bubble
    val tl = x.tail
    tl.foldLeft(hd)(directProduct)
  }

  // computes the duration of a set of time points relative to a cycle.
  def computeDur[A](tps: Pat[A], cycle: Pat[A])(implicit num: Num[A]): Pat[A] = {
    val one  = Constant(num.one)
    val dur0 = tps.differentiate
    val dur  = ((dur0 - one) mod cycle) + one
    dur.sum
  }

  // computes and sorts all possible sub patterns of a pattern
  def computeDurs[A](pattern: Pat[A], cantus: Pat[A], start: Pat[Int] = 0): Pat[Int] = {
    val positions = extract(cantus, pattern)
    val tuples0   = allTuples(positions)
    val tuples    = tuples0.sortWith { (a, b) =>
      val ad = computeDur(a, 7)
      val bd = computeDur(b, 7)
      ad > bd
    }
    val cantusSz  = cantus.size
    val clump     = ((start mod cantusSz) ++ tuples.flatten).sliding(2)
    clump.flatMap { pr =>
      val (pr0, pr1) = pr.splitAt(1)
      ((pr1 - pr0 - 1) mod cantusSz) + 1
    }
  }

  def makePart[A](pattern: Pat[A], cantus: Pat[A], rest: A, start: Pat[Int] = 0,
                  stutter: Pat[Int] = 1): (Pat[A], Pat[Double]) = {
    val durs0   = computeDurs(pattern, cantus, start).toDouble
    val durs    = durs0  .grouped(stutter).stutter(stutter).flatten / stutter.hold()
    val post    = pattern.grouped(stutter).stutter(stutter).flatten
    val pre     = Constant(rest).take(stutter)
    val ptrnOut = (pre ++ post).loop()
    (ptrnOut, durs * 0.02)
  }

  def mkGraph[Tx](): Pat[Event] = {

    def mkNotes(in: Pat[Int]): Pat[Double] = in * 2.4 + 4.0

    val baseBind: Bind.Map = Map(
      "proc"    -> "sine4",
      "octave"  -> 5,
      "detune"  -> White(-2.0, 2.0),
      "dr"      -> 0.1,
      "stretch" -> 1
    )

    def catPat(cantus: Pat[Int]): Pat[Event] = {
      val map = baseBind ++ Bind.Map(
        "note"  -> mkNotes(cantus),
        "rest"  -> (cantus sig_== -100),
        "dur"   -> 0.2,
        "db"    -> -45,
        "pan"   -> 0,
        "out"   -> White(0, 23),
        "i"     -> ArithmSeq(0, 1).mod(24),
        "ar"    -> 0.001
      )
      Bind(map)
    }

    val lPat = Pat.loop()((8 to 12).mirror)
    val rPat = Pat.loop()((5 to  8).mirror) / 25.0

    val xs = for {
      len     <- lPat.bubble
      rests   <- rPat.bubble
      offset  <- ArithmSeq(0, 1).bubble
      cantus0 <- Brown(-6, 6, 3).grouped(len)
    } yield {
      val numPause    = (len * rests).toInt
//      val cantus      = Pat.fold(cantus0, numPause)(_.updated(len.rand, -100))
      val cantus      = cantus0.updatedAll(len.hold().take(numPause).rand, -100)
      val cantusEvt   = catPat(cantus)
      val pitchSets0  = cantus.distinct.sorted.combinations(3)
      val pitchSets   = pitchSets0.map(_.shuffle)
      val numParts    = pitchSets.size.hold()

      val pats        = pitchSets.mapWithIndex { (part, partsIdx) =>
        val partsIdxH = partsIdx.hold()
        val (notePat, durPat) = makePart(part, cantus, rest = -100, stutter = Pat(1, 1, 2, 2, 4).choose)
        val map = baseBind ++ Bind.Map(
          "note"    -> mkNotes(notePat),
          "rest"    -> (notePat sig_== -100),
          "dur"     -> durPat,
          "legato"  -> partsIdxH.linlin(0, numParts, 0.02, 1.0),
          "i"       -> (partsIdxH + offset).mod(24),
          "ar"      -> 0.001,
          "db"      -> partsIdxH.linlin(0, numParts, -40.0, -30.0)
        )
        Bind(map)
      }

      Par(pats :+ cantusEvt)
    }
    xs.flatten
  }
}