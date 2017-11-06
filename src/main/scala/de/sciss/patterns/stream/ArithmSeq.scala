package de.sciss.patterns
package stream

import scala.collection.AbstractIterator
import scala.collection.immutable.{IndexedSeq => Vec}

final class ArithmSeq[A](start: Stream[A], step: Stream[A], length: Stream[Int])(implicit num: Numeric[A])
  extends Stream[A] {

  def name: String = productPrefix

  def inputs: Vec[Stream[_]] = Vector(start, step, length)

  private[patterns] val iterator: Iterator[A] = {
    val startIt = start .iterator
    val stepIt  = step  .iterator
    val lenIt   = length.iterator
    if (startIt.isEmpty || stepIt.isEmpty || lenIt.isEmpty) Iterator.empty
    else {
      val startVal  = startIt.next()
      val lenVal    = lenIt  .next()

      new AbstractIterator[A] {
        private[this] var counter = 0
        private[this] var state   = startVal
        private[this] var stepVal: A = _

        def hasNext: Boolean = counter < lenVal && stepIt.hasNext

        def next(): A = {
          if (counter >= lenVal) throw new NoSuchElementException("next on empty iterator")
          val res  = state
          stepVal  = stepIt.next()
          state    = num.plus(res, stepVal)
          counter += 1
          res
        }
      }
//      Iterator.iterate(startVal)(i => num.plus(i, stepVal)).take(lenIt.next())
    }
  }

  def aux: List[Stream.Aux] = Nil
}
