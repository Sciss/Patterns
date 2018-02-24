package de.sciss.patterns
package graph

final case class PatSeq[A](elems: A*) extends Pattern[A] {
  private def simpleString: String =
    s"Pat(${elems.iterator.take(4).mkString(", ")}).iterator"

  override def toString: String = simpleString

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    logStream(simpleString)
    Stream(elems: _*)
  }

  def transform(t: Transform): Pat[A] = {
    val elemsT: Seq[_] = elems.map {
      case e: Pat[_]  => t(e)
      case e          => e
    }
    val elemsC = elemsT.asInstanceOf[Seq[A]]
    PatSeq(elemsC: _*)
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
