package de.sciss.patterns
package graph
package impl

import de.sciss.patterns.Types.{Aux, Top}

import scala.collection.immutable.{Seq => ISeq}

final case class PatFromSeq[T <: Top](xs: ISeq[T#Out]) extends Pat[T] {
  private[patterns] def expand[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out] = iterator
  def embed                   [Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out] = iterator

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out] = new Stream[Tx, T#Out] {
    private[this] val size  = xs.size
    private[this] val idx   = ctx.newVar(0)

    // shouldn't be called, as this pattern is only created at runtime
    def reset()(implicit tx: Tx): Unit = throw new UnsupportedOperationException

    def hasNext(implicit tx: Tx): Boolean = idx() < size

    def next()(implicit tx: Tx): T#Out = {
      if (!hasNext) Stream.exhausted()
      val _idx  = idx()
      val res   = xs(_idx)
      idx()     = _idx + 1
      res
    }
  }

  private[patterns] def aux: List[Aux] = Nil
}
