package de.sciss.patterns

class SeqFillDebug extends PatSpec {
  "SeqFill" should work in {
    showStreamLog = true

    val pat = Graph {
      Pat.seqFill(4) { i =>
        i + i // .take(1)
      }
    }

    val plain = (0 until 4).map(i => i + i)

    eval(pat) shouldBe plain
  }
}