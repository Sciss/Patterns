package de.sciss.patterns
package graph

import de.sciss.patterns.Types.CTop

final case class PatSeq[T <: CTop](elems: Seq[T#COut]) extends Pattern[T] {
  private def simpleString: String =
    s"PatSeq(${elems.iterator.take(4).mkString(", ")}).iterator"

  override def toString: String = simpleString

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#COut] = {
    logStream(simpleString)
    Stream(elems: _*)
  }
//
//  private final class StreamImpl[Tx](implicit ctx: Context[Tx]) extends Stream[Tx, T#COut] {
//    private[this] val count = ctx.newVar(0)
//    private[this] val xs    = elems.toIndexedSeq
//
//    override def toString = s"$simpleString; count = $count"
//
//    def reset()(implicit tx: Tx): Unit    = count() = 0
//    def hasNext(implicit tx: Tx): Boolean = count() < xs.size
//
//    def next ()(implicit tx: Tx): T#COut = {
//      if (!hasNext) Stream.exhausted()
//      val i = count()
//      count() = i + 1
//      val res = elems(i)
//      logStream(s"$simpleString.next(); count = $i; res = $res")
//      res
//    }
//  }
}
