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
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{Disposable, Obj, Source, Txn => LTxn}
import de.sciss.patterns
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute.View
import de.sciss.synth.proc.{AuralAttribute, AuralContext, Pattern}

/*
  XXX TODO: some DRY with AuralGraphemeBase

 */
object AuralPatternAttribute extends AuralAttribute.Factory {
  type Repr[T <: LTxn[T]] = Pattern[T]

  def tpe: Obj.Type = Pattern

  private[this] lazy val _init: Unit = AuralAttribute.addFactory(this)

  def init(): Unit = _init

  def apply[T <: Txn[T]](key: String, pat: Pattern[T], observer: AuralAttribute.Observer[T])
                        (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val res = prepare[T, tx.I](key, pat, observer)(tx, tx.inMemoryBridge, context)
    res.init(pat)
  }

  private def prepare[T <: Txn[T], I1 <: LTxn[I1]](key: String, value: Pattern[T],
                                                      observer: AuralAttribute.Observer[T])
                                                     (implicit tx: T, iSys: T => I1,
                                                      context: AuralContext[T]): AuralPatternAttribute[T, I1] = {
//    implicit val iSys: T => I1 = system.inMemoryTx

    val tree = AuralStreamLikeAttribute.mkTree[T, I1]()
    new AuralPatternAttribute[T, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralPatternAttribute[T <: Txn[T], I <: LTxn[I]](key: String,
                                                             objH: Source[T, Pattern[T]],
                                                             observer: AuralAttribute.Observer[T],
                                                             tree: SkipList.Map[I, Long, View[T]])
                                                            (implicit context: AuralContext[T],
                                                                iSys: T => I)
  extends AuralStreamLikeAttribute[T, I, Pattern[T]](key, objH, observer, tree)
    with AuralAttribute[T] {
  attr =>

  def tpe: Obj.Type = Pattern

  private[this] var patObserver: Disposable[T] = _

  protected type St = (patterns.Stream[I, Any], patterns.lucre.Context[T, I])

  protected def disposeStream(st: St)(implicit tx: T): Unit = {
    implicit val itx: I = iSys(tx)
    st._1.dispose()
  }

  protected def makeStream(patObj: Pattern[T])(implicit tx: T): St = {
    val _ctx  = patterns.lucre.Context.dual[T, I](patObj)
    val _st   = _ctx.expandDual(patObj.value)
    (_st, _ctx)
  }

  protected def streamHasNext(st: St)(implicit tx: T): Boolean =
    st._1.hasNext(st._2, iSys(tx))

  protected def streamNext(st: St)(implicit tx: T): Any =
    st._1.next()(st._2, iSys(tx))

  def init(pat: Pattern[T])(implicit tx: T): this.type = {
    val graph0 = pat.value
    setRepr(graph0)
    patObserver = pat.changed.react { implicit tx => upd =>
      setRepr(upd.now)
    }
    this
  }

  override def dispose()(implicit tx: T): Unit = {
    patObserver.dispose()
    super.dispose()
  }
}