package de.sciss.patterns

trait Random[-Tx] {
  def nextDouble()(implicit tx: Tx): Double
  def nextLong  ()(implicit tx: Tx): Long

  def nextInt(n: Int)(implicit tx: Tx): Int
}