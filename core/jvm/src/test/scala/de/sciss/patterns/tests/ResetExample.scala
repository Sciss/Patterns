package de.sciss.patterns.tests

import de.sciss.lucre.Plain
import de.sciss.patterns.PatImport._
import de.sciss.patterns.{Context, Graph, graph}
import de.sciss.synth.swing.Plotting.Implicits._

import scala.swing.Swing

/*

  Reset a brownian walk every ten steps, four times.

  ex1: Brown is inside the reset scope
  ex2: Brown is outside the reset scope

 */
object ResetExample {
  def main(args: Array[String]): Unit = {

    import graph._

    val ex1 = Graph {
      Pat.loop(4) {
        val b = Brown(0, 100, 2)
        b.take(10)
      }
    }

    val ex2 = Graph {
      val b = Brown(0, 100, 2)
      Pat.loop(4) {
        b.take(10)
      }
    }

    implicit val ctx: Context[Plain] = Context()

    def plot(g: Pat[Int], title: String): Unit =
      g.expand.toList.plot(title = title, discrete = true)

    Swing.onEDT {
      plot(ex1, "ex1")
      plot(ex2, "ex2")
    }
  }
}
