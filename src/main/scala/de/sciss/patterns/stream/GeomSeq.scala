package de.sciss.patterns
package stream

import de.sciss.patterns.PE.Value

import scala.collection.AbstractIterator
import scala.collection.immutable.{IndexedSeq => Vec}

final class GeomSeq[A <: PE.Numeric](start: Stream[A], grow: Stream[A], length: Stream[Value.Int])
  extends Stream[A] {

  def name  : String            = "GeomSeq"
  def inputs: Vec[Stream[_]]    = Vector(start, grow, length)
  def aux   : List[Stream.Aux]  = Nil

  val tpe: A = start.tpe

  private[patterns] val iterator: Iterator[tpe.Out] = {
    val startIt = start .iterator
    val growIt  = grow  .iterator
    val lenIt   = length.iterator
    if (startIt.isEmpty || growIt.isEmpty || lenIt.isEmpty) Iterator.empty
    else {
      val startVal  = startIt.next()
      val lenVal    = lenIt  .next()

      new AbstractIterator[tpe.Out] {
        private[this] var counter = 0
        private[this] var state  : tpe.Out = startVal.asInstanceOf[tpe.Out]
        private[this] var growVal: tpe.Out = _

        def hasNext: Boolean = counter < lenVal && growIt.hasNext

        def next(): tpe.Out = {
          if (counter >= lenVal) throw new NoSuchElementException("next on empty iterator")
          val res  = state
          growVal  = growIt.next().asInstanceOf[tpe.Out]
          state    = tpe.num.times(res, growVal)
          counter += 1
          res
        }
      }
    }
  }
}
