package de.sciss.patterns

import de.sciss.patterns.Types.Top
import graph._

class ExtractDebug extends PatSpec {
  "The directProduct example" should work in {
    // collects the indices of every occurrence of elements of t in s
    def extract_Sq[A](s: Seq[A], t: Seq[A]): Seq[Seq[Int]] =
      t.map { tj =>
        s.zipWithIndex.collect {
          case (b, i) if b == tj => i
        }
      }

    // collects the indices of every occurrence of elements of t in s
    def extract_Pat[A <: Top](s: Pat[A], t: Pat[A]): Pat[Pat.Int] =
      t.bubble.map { tj: Pat[A] =>
        val indices   = s.recur().indexOfSlice(tj)
//        val indicesF  = indices.bubbleFilter(_ >= 0)
        val indicesF  = FilterSeq(indices, indices >= 0)
        indicesF
      }

    val as  = Seq(1, 5, 2, 3, 4)
    val bs  = Seq(4, 5, 6)
    val plain = extract_Sq(as, bs)
    assert(plain === List(List(4), List(1), List()))

    val pat = Graph {
      extract_Pat(Pat.Int(as: _*), Pat.Int(bs: _*))
    }

    evalH(pat) shouldBe plain
  }
}
