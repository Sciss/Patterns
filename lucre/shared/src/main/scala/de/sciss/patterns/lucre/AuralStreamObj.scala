/*
 *  AuralStreamObj.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongPoint2DLike, LongSquare}
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{Obj, Source, Txn => LTxn}
import de.sciss.patterns.lucre.impl.AuralStreamLikeObj
import de.sciss.span.SpanLike
import de.sciss.synth.UGenSource.Vec
import de.sciss.proc.{AuralContext, AuralObj, Runner}

object AuralStreamObj extends AuralObj.Factory {
  def tpe: Obj.Type = Stream

  type Repr[~ <: LTxn[~]] = Stream[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[T <: Txn[T]](st: Stream[T], attr: Runner.Attr[T]
                        )(implicit tx: T, context: AuralContext[T]): AuralObj[T] = {
    // XXX TODO --- pass on `attr`
    val res = prepare[T, tx.I](st)(tx, tx.inMemoryBridge, context)
    res.init(st)
  }

  private def prepare[T <: Txn[T], I1 <: LTxn[I1]](value: Stream[T])
                                                     (implicit tx: T, iSys: T => I1,
                                                      context: AuralContext[T]): AuralStreamObj[T, I1] = {
    val tree = AuralStreamLikeObj.mkTree[T, I1]()
    new AuralStreamObj[T, I1](tx.newHandle(value), tree)
  }
}
final class AuralStreamObj[T <: Txn[T], I1 <: LTxn[I1]](objH: Source[T, Stream[T]],
                                                            tree: SkipOctree[I1, LongPoint2DLike, LongSquare, (SpanLike, Vec[AuralObj[T]])]
                                                           )
                                                           (implicit context: AuralContext[T],
                                                            protected val iSys: T => I1)
  extends AuralStreamLikeObj[T, I1, Stream[T]](objH, tree) {
  attr =>

  def tpe: Obj.Type = Stream

  protected type St = Stream[T]

  def init(st: Stream[T])(implicit tx: T): this.type = {
    this
  }

  protected def disposeStream(st: St)(implicit tx: T): Unit = ()

  protected def makeStream(stObj: Stream[T])(implicit tx: T): St = stObj

  protected def streamHasNext(st: Stream[T])(implicit tx: T): Boolean =
    st.peer().hasNext(st.context, tx)

  protected def streamNext(st: Stream[T])(implicit tx: T): Any =
    st.peer().next()(st.context, tx)
}