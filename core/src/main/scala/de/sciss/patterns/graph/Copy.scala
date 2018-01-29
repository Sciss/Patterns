package de.sciss.patterns
package graph

import de.sciss.patterns.Types.Top

final case class Copy[T <: Top](in: Pat[T]) extends Pattern[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new Stream[Tx, T#Out[Tx]] {
    private[this] val peer = in.expand[Tx]

    def reset()(implicit tx: Tx): Unit      = peer.reset()
    def hasNext(implicit tx: Tx): Boolean   = peer.hasNext
    def next()(implicit tx: Tx): T#Out[Tx]  = peer.next()
  }
}
