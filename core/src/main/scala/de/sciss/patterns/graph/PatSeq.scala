package de.sciss.patterns
package graph

import de.sciss.patterns.Types.CTop

final case class PatSeq[T <: CTop](elems: Seq[T#COut]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#COut] = new Stream[Tx, T#COut] {
    private[this] val count = ctx.newVar(0)
    private[this] val xs    = elems.toIndexedSeq

    def reset()(implicit tx: Tx): Unit    = count() = 0
    def hasNext(implicit tx: Tx): Boolean = count() < xs.size

    def next ()(implicit tx: Tx): T#COut = {
      if (!hasNext) Stream.exhausted()
      val i = count()
      count() = i + 1
      elems(i)
    }
  }
}
