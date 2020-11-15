package de.sciss.patterns.lucre

import de.sciss.lucre.Txn
import de.sciss.patterns
import de.sciss.patterns.lucre.MacroImplicits._
import de.sciss.synth.proc

trait MacroTest[T <: Txn[T]] {
  def make()(implicit tx: T): Unit = {
    val pat = proc.Pattern[T]()
    import de.sciss.patterns.graph._
    import de.sciss.patterns.PatImport._
    pat.setGraph {
      val b = Brown(0, 127, 3)
      b
    }

    val str = patterns.lucre.Stream[T]()
    str.setGraph {
      val b = Brown(0, 127, 3)
      b
    }
  }
}
