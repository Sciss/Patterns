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

    private def inNextLevel[R](isCarry: Boolean)(fun: Int => R)(implicit tx: Tx): R = {
      val lvl = levelVar.get() - 1
      if (lvl >= 0) fun(lvl) else {
        validate()
        val vec = outerVec()
        println(s"AQUI. vec.size = ${vec.size}")
        (new Exception).fillInStackTrace().printStackTrace(System.out)
        val lvl0  = vec.size
        val lvl   = if (false && isCarry) lvl0 else lvl0 - 1
        useLevel(lvl0)(fun(lvl))
      }
    }

    private def useLevel[R](lvl: Int)(body: => R): R = {
      logStream(s"FoldLeft.useLevel($lvl) >>")
      val prev = levelVar.get()
      levelVar.set(lvl)
      try {
        body
      } finally {
        levelVar.set(prev)
        logStream(s"FoldLeft.useLevel($lvl) <<")
      }
    }

    private def mkItInStream(implicit tx: Tx): Stream[Tx, B] =
      inNextLevel(isCarry = false) { lvl =>
        logStream(s"FoldLeft.iterator.mkItInStream - lvl $lvl")
        outerVec().apply(lvl).expand
      }

    private def mkItCarryStream(implicit tx: Tx): Stream[Tx, A] =
      inNextLevel(isCarry = true) { lvl =>
        mkItCarryStream(lvl)
      }

    private def mkItCarryStream(lvl: Int)(implicit tx: Tx): Stream[Tx, A] = {
      logStream(s"FoldLeft.iterator.mkItCarryStream - lvl $lvl")
      if (lvl == 0) zStream else {
        val res = new CarryStream(lvl, tx)
        //        ctx.addStream(refCarry, res)
        //        buf.addIt(res)
        res
      }
    }

//    private final class CarryStream(lvl: Int, tx0: Tx) extends Stream[Tx, A] {
//      private[this] val innerStream = ctx.newVar[Stream[Tx, A]](null)
//      private[this] val _hasInner   = ctx.newVar(false)
//
//      private def use[R](body: Stream[Tx, A] => R)(implicit tx: Tx): R = useLevel(lvl) {
//        if (!_hasInner()) {
//          _hasInner()   = true
//          innerStream() = inner.expand(ctx, tx0)
//        }
//        body(innerStream())
//      }
//
//      def reset()(implicit tx: Tx): Unit    = ()
//      def hasNext(implicit tx: Tx): Boolean = use(_.hasNext)
//      def next() (implicit tx: Tx): A       = use(_.next())
//    }

    private final class CarryStream(lvl: Int, tx0: Tx) extends Stream[Tx, A] {
      override def toString = s"FoldLeft.CarryStream($lvl)"

      private def use[R](body: => R): R = useLevel(lvl)(body)

      private[this] val innerStream = use(inner.expand(ctx, tx0))

      def reset()(implicit tx: Tx): Unit    = ()
      def hasNext(implicit tx: Tx): Boolean = use(innerStream.hasNext)
      def next() (implicit tx: Tx): A       = {
        val res = use(innerStream.next())
        logStream(s"FoldLeft.CarryStream($lvl).next() = $res")
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
        val res       = mkItCarryStream(numLevels)
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
      val s = _result()
      s.hasNext
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val s = _result()
      val res = s.next()
      logStream(s"FoldLeft.iterator.next() = $res")
      res
    }
  }
}