package de.sciss.patterns

import de.sciss.synth.swing.Plotting.Implicits._

import scala.swing.Swing

/*

  Reset a brownian walk every ten steps, four times

 */
object ResetExample {
  def main(args: Array[String]): Unit = {

    import graph._

    val outer = Graph {
      Pat.seqFill(4) { _ =>
        val b = Brown(0, 100, 2)
        b.take(10)
      }
    }

    implicit val ctx: Context = Context()
    outer.iterator.foreach(println)

//    Swing.onEDT {
//      outer.iterator.toList.plot(discrete = true)
//    }
  }
}
