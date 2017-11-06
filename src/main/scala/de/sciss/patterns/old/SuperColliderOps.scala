package de.sciss.patterns.old

import scala.util.Random

object SuperColliderOps {
  def rrand(lo: Int, hi: Int)(implicit rnd: Random): Int =
    rnd.nextInt(hi - lo + 1) + lo

  implicit final class IntRandOps(private val i: Int) extends AnyVal {
    def rand(implicit rnd: Random): Int = rnd.nextInt(i)
    def iterate(fun: Int => Any): Unit = {
      var j = 0
      while (j < i) {
        fun(j)
        j += 1
      }
    }

    def isEven: Boolean = i % 2 == 0
    def isOdd : Boolean = i % 2 == 1
  }
}
