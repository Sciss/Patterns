package de.sciss.patterns.lucre

import de.sciss.lucre.Txn
import de.sciss.patterns
import de.sciss.patterns.lucre.MacroImplicits._

trait MacroTest[T <: Txn[T]] {
  def make()(implicit tx: T): Unit = {
    val pat = patterns.lucre.Pattern[T]()
    import de.sciss.patterns.graph._
    pat.setGraph {
      val b = Brown(0, 127, 3)
      b
    }

    val str = patterns.lucre.Stream[T]()
    import de.sciss.patterns.graph._
    str.setGraph {
      val b = Brown(0, 127, 3)
      b
    }
  }
}
