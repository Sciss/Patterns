package de.sciss.patterns

import de.sciss.patterns.graph.impl.TimSort
import org.scalatest.{FlatSpec, Matchers}

class TimSortSpec extends FlatSpec with Matchers {
  "TimSort" should "work" in {
    val rnd = new util.Random(0L)
    val a1  = Array.fill[Int](1000)(rnd.nextInt)
    val a1s = a1.sorted
    val a1t = a1.clone()
    TimSort.sort(a1t)
    val a2t = a1t.clone()
    TimSort.sort(a2t)
    val a3t = a1s.reverse
    TimSort.sort(a3t)
    assert(a1s === a1t)
    assert(a1s === a2t)
    assert(a1s === a3t)
  }
}
