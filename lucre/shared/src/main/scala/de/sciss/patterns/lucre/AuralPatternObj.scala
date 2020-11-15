/*
 *  AuralPatternObj.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.{Source, Obj => LObj, Txn => LTxn}
import de.sciss.patterns
import de.sciss.patterns.lucre.impl.AuralStreamLikeObj
import de.sciss.span.SpanLike
import de.sciss.synth.UGenSource.Vec
import de.sciss.proc.{AuralContext, AuralObj, Pattern, Runner}

object AuralPatternObj extends AuralObj.Factory {
  def tpe: LObj.Type = Pattern

  type Repr[~ <: LTxn[~]] = Pattern[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[T <: Txn[T]](pat: Pattern[T], attr: Runner.Attr[T]
                        )(implicit tx: T, context: AuralContext[T]): AuralObj[T] = {
    // XXX TODO --- pass on `attr`
    val res = prepare[T, tx.I](pat)(tx, tx.inMemoryBridge, context)
    res.init(pat)
  }

  private def prepare[T <: Txn[T], I1 <: LTxn[I1]](value: Pattern[T])
                                                     (implicit tx: T, iSys: T => I1,
                                                      context: AuralContext[T]): AuralPatternObj[T, I1] = {
    val tree = AuralStreamLikeObj.mkTree[T, I1]()
    new AuralPatternObj[T, I1](tx.newHandle(value), tree)
  }
}
final class AuralPatternObj[T <: Txn[T], I1 <: LTxn[I1]](objH: Source[T, Pattern[T]],
                                                            tree: SkipOctree[I1, LongPoint2DLike, LongSquare, (SpanLike, Vec[AuralObj[T]])]
                                                           )
                                                           (implicit context: AuralContext[T],
                                                            protected val iSys: T => I1)
  extends AuralStreamLikeObj[T, I1, Pattern[T]](objH, tree) {
  attr =>

  def tpe: LObj.Type = Pattern

  protected type St = (patterns.Stream[I1, Any], patterns.lucre.Context[T, I1])

//  private[this] var patObserver: Disposable[T] = _

//  private def setPattern(g: Pat[_])(implicit tx: T): Unit = {
//    implicit val itx: I1#Tx = iSys(tx)
//    playingRef.foreach(elemRemoved(_, elemPlays = true))(tx.peer)
//    tree.clear()
//    disposeStream()
//  }

  def init(pat: Pattern[T])(implicit tx: T): this.type = {
    this
  }

  protected def disposeStream(st: St)(implicit tx: T): Unit = {
    implicit val itx: I1 = iSys(tx)
    st._1.dispose()
  }

  protected def makeStream(patObj: Pattern[T])(implicit tx: T): St = {
    val _ctx  = patterns.lucre.Context.dual[T, I1](patObj)
    val _st   = _ctx.expandDual(patObj.value) // g.expand[I1]
    (_st, _ctx)
  }

  protected def streamHasNext(st: St)(implicit tx: T): Boolean =
    st._1.hasNext(st._2, iSys(tx))

  protected def streamNext(st: St)(implicit tx: T): Any =
    st._1.next()(st._2, iSys(tx))
}