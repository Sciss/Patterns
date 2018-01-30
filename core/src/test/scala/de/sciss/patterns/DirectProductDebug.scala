package de.sciss.patterns

import de.sciss.patterns.Types.{IntTop, Top}

class DirectProductDebug extends PatSpec {
  "The directProduct example" should work in {
    showStreamLog = true

    def directProduct_Seq[A](a: Seq[Seq[A]], b: Seq[A]): Seq[Seq[A]] =
      a.flatMap { v => b.map { w => v :+ w } }

    //    def directProduct_Pat_NO_WAY_JOSÉ[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
    //      a.map { v: Pat[A] =>
    //        val bc = b.copy()
    //        bc.bubbleMap { w =>
    //          v ++ w
    //          // bc.take(1) // v.copy() ++ bc.take(1) // w
    //        }
    //      }

    val aInSeq = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val bInSeq = Seq(7, 8)
    val plain = directProduct_Seq(aInSeq, bInSeq)
    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))

    def directProduct_Pat[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
      a.flatMap { v: Pat[A] =>
        val bc = b.copy()
        bc.bubble.map { w: Pat[A] =>
          v ++ w
          // bc.take(1) // v.copy() ++ bc.take(1) // w
        }
      }

    def directProduct_Pat1() = {
      val a: Pat[Pat.Int] = aInSeq.map(xs => Pat[IntTop](xs: _*))
      // a.flatMap { v: Pat.Int =>
        val bc: Pat.Int = Pat[IntTop](bInSeq: _*)
        bc.bubble.map { w: Pat.Int =>
          w // v // ++ w
          // bc.take(1) // v.copy() ++ bc.take(1) // w
        }
      // }
    }

    def directProduct_Pat2() = {
      val a: Pat[Pat.Int] = aInSeq.map(xs => Pat[IntTop](xs: _*))
      a.flatMap { v: Pat.Int =>
        val bc: Pat.Int = Pat[IntTop](bInSeq: _*)
        bc.bubble.map { w: Pat.Int =>
          val vc = v.copy()
          vc ++ w
        // bc.take(1) // v.copy() ++ bc.take(1) // w
        }
      }
    }

    //    val outPat = Graph {
//      val aInPat: Pat[Pat.Int]  = aInSeq.map(xs => Pat[IntTop](xs: _*))
//      val bInPat: Pat.Int       = Pat[IntTop](bInSeq: _*)
//      directProduct_Pat(aInPat, bInPat)
//    }
//
//    val outPat = Graph {
//      directProduct_Pat1()
//    }

    val outPat = Graph {
      directProduct_Pat2()
    }

    import ctx.tx
    val res = outPat.expand.map { in =>
      val i = in.toList
      i
    }.toList

    assert(res === plain)  // XXX TODO fails

    /*
      Se we have

        List(List(1, 2, 3, 7), List(8), List(4, 5, 6, 7), List(8))

      instead of

        List(List(1, 2, 3, 7), List(1, 2, 3, 8), List(4, 5, 6, 7), List(4, 5, 6, 8))

      if we create a copy of `v`, the output is

        List(List(1, 2, 3, 7), List(1, 2, 3, 8), List(1, 2, 3, 7), List(1, 2, 3, 8))

     */
  }
}