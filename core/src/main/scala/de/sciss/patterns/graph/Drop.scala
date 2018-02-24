package de.sciss.patterns
package graph

import de.sciss.patterns.graph.impl.TruncateStream

final case class Drop[A](in: Pat[A], length: Pat[Int]) extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  def transform(t: Transform): Pat[A] = {
    val inT     = t(in)
    val lengthT = t(length)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends TruncateStream[A, Tx](in, length, tx0) {

    protected def truncate(it: Stream[Tx, A], n: Int)(implicit tx: Tx): Stream[Tx, A] =
      it.drop(n)
  }
}