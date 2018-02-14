/*
 *  FoldLeft.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

final case class FoldLeft[B, A](outer: Pat[Pat[B]], z: Pat[A], itIn: It[B], itCarry: It[A],
                                inner: Pat[A])
  extends Pattern[A] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
//    @transient final private[this] lazy val refIn     = new AnyRef
//    @transient final private[this] lazy val refCarry  = new AnyRef

    ctx.provideOuterStream[B](itIn   .token, mkItInStream   (_))(tx0)
    ctx.provideOuterStream[A](itCarry.token, mkItCarryStream(_))(tx0)

    private[this] val outerStream   = outer .expand(ctx, tx0)
    private[this] val zStream       = z     .expand(ctx, tx0)

    private[this] val _valid        = ctx.newVar(false)
    private[this] val _result       = ctx.newVar[Stream[Tx, A]](null)

    private[this] val outerVec      = ctx.newVar[Vector[Pat[B]]](null)

    private[this] val levelVar      = new ThreadLocal[Int]

    private def level(): Int = levelVar.get()

    private def useLevel[R](lvl: Int)(body: => R): R = {
      val prev = levelVar.get()
      levelVar.set(lvl)
      try {
        body
      } finally {
        levelVar.set(prev)
      }
    }

    private def mkItInStream(implicit tx: Tx): Stream[Tx, B] = {
      val lvl = level() - 1
      logStream(s"FoldLeft.iterator.mkItInStream - lvl $lvl")
      outerVec().apply(lvl).expand
    }

    private final class CarryStream(lvl: Int, tx0: Tx) extends Stream[Tx, A] {
      private[this] val innerStream = useLevel(lvl)(inner.expand(ctx, tx0))

      def reset()(implicit tx: Tx): Unit    = ()
      def hasNext(implicit tx: Tx): Boolean = useLevel(lvl)(innerStream.hasNext)
      def next() (implicit tx: Tx): A       = useLevel(lvl)(innerStream.next())
    }

    private def mkItCarryStream(implicit tx: Tx): Stream[Tx, A] = {
      val lvl = level() - 1
      logStream(s"FoldLeft.iterator.mkItCarryStream - lvl $lvl")
      if (lvl == 0) zStream else {
        val res = new CarryStream(lvl, tx)
//        ctx.addStream(refCarry, res)
//        buf.addIt(res)
        res
      }
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        logStream("FoldLeft.iterator.validate()")
        val _outer    = outerStream.toVector
        outerVec()    = _outer
        val numLevels = _outer.size
        val res       = useLevel(numLevels)(mkItCarryStream)
        _result()     = res
      }

    def reset()(implicit tx: Tx): Unit = {
      logStream("FoldLeft.iterator.reset()")
      _valid() = false
//      buf.clear()
//      ctx.getStreams(refIn).foreach {
//        case m: MapItStream[Tx, _] => m.resetOuter()
//      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _result().hasNext
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = _result().next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}