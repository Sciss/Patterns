package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base

final case class PatSeq[A](elems: A*) extends Pattern[A] {
  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
      else xs.mkString(", ")
    s"Pat($es)"
  }

  override def toString: String = simpleString

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    logStream(simpleString)
    Stream(elems: _*)
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val elemsT: Seq[_] = elems.map {
      case e: Pat[_]  => t(e)
      case e          => e
    }
    val elemsC = elemsT.asInstanceOf[Seq[A]]
    PatSeq(elemsC: _*)
  }

//
//  private final class StreamImpl[S <: Base[S]](implicit ctx: Context[S]) extends Stream[S, T#COut] {
//    private[this] val count = ctx.newVar(0)
//    private[this] val xs    = elems.toIndexedSeq
//
//    override def toString = s"$simpleString; count = $count"
//
//    def reset()(implicit tx: S#Tx): Unit    = count() = 0
//    def hasNext(implicit tx: S#Tx): Boolean = count() < xs.size
//
//    def next ()(implicit tx: S#Tx): T#COut = {
//      if (!hasNext) Stream.exhausted()
//      val i = count()
//      count() = i + 1
//      val res = elems(i)
//      logStream(s"$simpleString.next(); count = $i; res = $res")
//      res
//    }
//  }
}
