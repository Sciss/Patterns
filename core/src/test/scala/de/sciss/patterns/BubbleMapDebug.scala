package de.sciss.patterns

import de.sciss.patterns.Types.{IntTop, Top}

class BubbleMapDebug extends PatSpec {
  "BubbleMap" should work in {
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

    def directProduct_Pat[A <: Top](a: Pat[Pat[A]], b: Pat[A]): Pat[Pat[A]] =
      a.flatMap { v: Pat[A] =>
        val bc = b.copy()
        bc.bubble.map { w: Pat[A] =>
          v ++ w
          // bc.take(1) // v.copy() ++ bc.take(1) // w
        }
      }

    val aInSeq  = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
    val bInSeq  = Seq(7, 8)
    val plain   = directProduct_Seq(aInSeq, bInSeq)
    assert(plain === Seq(Seq(1, 2, 3, 7), Seq(1, 2, 3, 8), Seq(4, 5, 6, 7), Seq(4, 5, 6, 8)))

    val aInPat: Pat[Pat.Int]  = aInSeq.map(xs => Pat[IntTop](xs: _*))
    val bInPat: Pat.Int       = Pat[IntTop](bInSeq: _*)
    val outPat = Graph { directProduct_Pat(aInPat, bInPat) }
    import ctx.tx
    val res = outPat.expand.map { in =>
      val i = in.toList
      i
    }.toList

    assert(res === plain)  // XXX TODO fails
  }
}