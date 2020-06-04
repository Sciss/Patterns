/*
 *  AuralStreamObj.scala
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
import de.sciss.patterns.lucre.impl.AuralStreamLikeObj
import de.sciss.span.SpanLike
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc.{AuralContext, AuralObj, Runner}

object AuralStreamObj extends AuralObj.Factory {
  def tpe: Obj.Type = Stream

  type Repr[~ <: stm.Sys[~]] = Stream[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](st: Stream[S], attr: Runner.Attr[S]
                        )(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val system  = tx.system
    // XXX TODO --- pass on `attr`
    val res     = prepare[S, system.I](st)(tx, system, context)
    res.init(st)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](value: Stream[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralStreamObj[S, I1] = {
    val tree = AuralStreamLikeObj.mkTree[S, I1]()
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx
    new AuralStreamObj[S, I1](tx.newHandle(value), tree)
  }
}
final class AuralStreamObj[S <: Sys[S], I1 <: stm.Sys[I1]](objH: stm.Source[S#Tx, Stream[S]],
                                                            tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[AuralObj[S]])]
                                                           )
                                                           (implicit context: AuralContext[S],
                                                            system: S { type I = I1 },
                                                            protected val iSys: S#Tx => I1#Tx)
  extends AuralStreamLikeObj[S, S, I1, Stream[S]](objH, tree) {
  attr =>

  def tpe: Obj.Type = Stream

  protected type St = Stream[S]

  def init(st: Stream[S])(implicit tx: S#Tx): this.type = {
    this
  }

  protected def disposeStream(st: St)(implicit tx: S#Tx): Unit = ()

  protected def makeStream(stObj: Stream[S])(implicit tx: S#Tx): St = stObj

  protected def streamHasNext(st: Stream[S])(implicit tx: S#Tx): Boolean =
    st.peer().hasNext(st.context, tx)

  protected def streamNext(st: Stream[S])(implicit tx: S#Tx): Any =
    st.peer().next()(st.context, tx)
}