package de.sciss.patterns.old

import org.coroutines._

object IteratorTest {
  def main(/* args: Array[String] */): Unit = {
    val co = coroutine { () =>
      yieldval(3)
      yieldval(8)
      yieldval(11)
      ()
    }

    val it: Stream[Int] = co
    while (it.hasNext) println(it.next())
    println("Done.")
  }
}