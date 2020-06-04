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
import de.sciss.lucre.geom.LongSpace
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.impl.AuralStreamLikeObj
import de.sciss.span.SpanLike
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc.{AuralContext, AuralObj, Runner}

object AuralPatternObj extends AuralObj.Factory {
  def tpe: Obj.Type = Pattern

  type Repr[~ <: stm.Sys[~]] = Pattern[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](pat: Pattern[S], attr: Runner.Attr[S]
                        )(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val system  = tx.system
    // XXX TODO --- pass on `attr`
    val res     = prepare[S, system.I](pat)(tx, system, context)
    res.init(pat)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](value: Pattern[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralPatternObj[S, I1] = {
    val tree = AuralStreamLikeObj.mkTree[S, I1]()
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx
    new AuralPatternObj[S, I1](tx.newHandle(value), tree)
  }
}
final class AuralPatternObj[S <: Sys[S], I1 <: stm.Sys[I1]](objH: stm.Source[S#Tx, Pattern[S]],
                                                            tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[AuralObj[S]])]
                                                           )
                                                           (implicit context: AuralContext[S],
                                                            system: S { type I = I1 },
                                                            protected val iSys: S#Tx => I1#Tx)
  extends AuralStreamLikeObj[S, I1, I1, Pattern[S]](objH, tree) {
  attr =>

  def tpe: Obj.Type = Pattern

  protected type St = (patterns.Stream[I1, Any], patterns.lucre.Context[S, I1])

//  private[this] var patObserver: Disposable[S#Tx] = _

//  private def setPattern(g: Pat[_])(implicit tx: S#Tx): Unit = {
//    implicit val itx: I1#Tx = iSys(tx)
//    playingRef.foreach(elemRemoved(_, elemPlays = true))(tx.peer)
//    tree.clear()
//    disposeStream()
//  }

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    this
  }

  protected def disposeStream(st: St)(implicit tx: S#Tx): Unit = {
    implicit val itx: I1#Tx = iSys(tx)
    st._1.dispose()
  }

  protected def makeStream(patObj: Pattern[S])(implicit tx: S#Tx): St = {
    val _ctx  = patterns.lucre.Context.dual[S](patObj)
    val _st   = _ctx.expandDual(patObj.value) // g.expand[I1]
    (_st, _ctx)
  }

  protected def streamHasNext(st: St)(implicit tx: S#Tx): Boolean =
    st._1.hasNext(st._2, iSys(tx))

  protected def streamNext(st: (patterns.Stream[I1, Any], Context[S, I1]))(implicit tx: S#Tx): Any =
    st._1.next()(st._2, iSys(tx))
}