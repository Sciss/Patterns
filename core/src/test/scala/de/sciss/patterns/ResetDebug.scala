package de.sciss.patterns

class ResetDebug extends PatSpec {
  "Map reset" should work in {
//    val pat = Graph {
//      val in  = Pat(Pat(0, 6, 7))
//      val inM = in.map(identity)
//      Repeat(inM, 2)
//    }

    val pat = Graph {
      Pat.flatFill(2) {
        val in  = Pat(Pat(0, 6, 7))
        val inM = in.map(identity)
        inM
      }
    }

    evalH(pat) shouldBe Seq(Seq(0, 6, 7), Seq(0, 6, 7))
  }
}
