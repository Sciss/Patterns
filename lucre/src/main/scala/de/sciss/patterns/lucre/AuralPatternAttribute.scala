/*
 *  AuralPatternAttribute.scala
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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute.View
import de.sciss.synth.proc.{AuralAttribute, AuralContext}

/*
  XXX TODO: some DRY with AuralGraphemeBase

 */
object AuralPatternAttribute extends AuralAttribute.Factory {
  type Repr[S <: stm.Sys[S]] = Pattern[S]

  def tpe: Obj.Type = Pattern

  private[this] lazy val _init: Unit = AuralAttribute.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](key: String, pat: Pattern[S], observer: AuralAttribute.Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](key, pat, observer)(tx, system, context) // IntelliJ highlight bug
    res.init(pat)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](key: String, value: Pattern[S],
                                                      observer: AuralAttribute.Observer[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralPatternAttribute[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx

    val tree = AuralStreamLikeAttribute.mkTree[S, I1]()
    new AuralPatternAttribute[S, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralPatternAttribute[S <: Sys[S], I1 <: stm.Sys[I1]](key: String,
                                                                  objH: stm.Source[S#Tx, Pattern[S]],
                                                                  observer: AuralAttribute.Observer[S],
                                                                  tree: SkipList.Map[I1, Long, View[S]])
                                                               (implicit context: AuralContext[S],
                                                                system: S { type I = I1 },
                                                                iSys: S#Tx => I1#Tx)
  extends AuralStreamLikeAttribute[S, I1, Pattern[S]](key, objH, observer, tree)
    with AuralAttribute[S] {
  attr =>

  def tpe: Obj.Type = Pattern

  private[this] var patObserver: Disposable[S#Tx] = _

  protected type St = (patterns.Stream[I1, Any], patterns.lucre.Context[S, I1])

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

  protected def streamNext(st: St)(implicit tx: S#Tx): Any =
    st._1.next()(st._2, iSys(tx))

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    val graph0 = pat.value
    setRepr(graph0)
    patObserver = pat.changed.react { implicit tx => upd =>
      setRepr(upd.now)
    }
    this
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    patObserver.dispose()
    super.dispose()
  }
}