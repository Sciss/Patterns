package de.sciss.patterns.lucre

import de.sciss.lucre.stm.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.MacroImplicits._

trait MacroTest[S <: Sys[S]] {
  def make()(implicit tx: S#Tx): Unit = {
    val pat = patterns.lucre.Pattern[S]()
    import de.sciss.patterns.graph._
    pat.setGraph {
      val b = Brown(0, 127, 3)
      b
    }

    val str = patterns.lucre.Stream[S]()
    import de.sciss.patterns.graph._
    str.setGraph {
      val b = Brown(0, 127, 3)
      b
    }
  }
}
