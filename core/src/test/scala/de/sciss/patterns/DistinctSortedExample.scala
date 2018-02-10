package de.sciss.patterns

import de.sciss.synth.swing.Plotting.Implicits._

import scala.swing.Swing

object DistinctSortedExample {
  def main(args: Array[String]): Unit = {

    import graph._

//    List(2,3,4,5).combinations(3)

    val ex1 = Graph {
      Pat.seqFill(4) { _ =>
        val b = Brown(0, 100, 2)
        val d = b.take(10).distinct.sorted
        d ++ (-1: Pat[Int]).head
      }
    }

    val ex2 = Graph {
      val b = Brown(0, 100, 2)
      Pat.seqFill(4) { _ =>
        val d = b.take(10).distinct.sorted
        d ++ (-1: Pat[Int]).head
      }
    }

    implicit val ctx: Context.Plain = Context()
    import ctx.tx

    def plot(g: Pat[Int], title: String): Unit =
      g.iterator.toList.plot(title = title, discrete = true)

    Swing.onEDT {
      plot(ex1, "ex1")
      plot(ex2, "ex2")
    }
  }
}
